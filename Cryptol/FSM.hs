{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveTraversable #-}
module Cryptol.FSM
  ( SimpleType(..)
  , fromSimpleType
  , toSimpleType
  , ExprBuilderParams
  , getExprBuilderParams
  , step
  , checkEquality
  ) where

import Control.Exception (assert)
import Cryptol.Eval.Type (evalType)
import Cryptol.ModuleSystem.Name (lookupPrimDecl)
import Cryptol.Parser (ParseError, parseSchema)
import Cryptol.Parser.Position (emptyRange)
import Cryptol.Symbolic (ProverCommand(ProverCommand), ProverResult(AllSatResult, ThmResult, ProverError), QueryType(SatQuery), SatNum(SomeSat), pcExpr, pcExtraDecls, pcProverName, pcQueryType, pcSchema, pcSmtFile, pcVerbose)
import Cryptol.TypeCheck.Solver.InfNat (Nat'(Nat))
import Cryptol.Utils.Ident (packIdent)
import Cryptol.ModuleM (ModuleM, checkExpr, getEvalEnv, getPrimMap, satProve, typeCheckInteractive)
import Cryptol.Utils.PP (pretty)
import Data.List (genericLength)
import Data.String (fromString)
import qualified Cryptol.Eval.Value    as E
import qualified Cryptol.Parser.AST    as P
import qualified Cryptol.TypeCheck.AST as TC

data SimpleType = SimpleType
  { inputBits  :: Integer
  , outputType :: E.TValue
  }

fromSimpleType :: SimpleType -> TC.Schema
fromSimpleType (SimpleType n out) = TC.Forall
  { TC.sVars  = []
  , TC.sProps = []
  , TC.sType  = TC.tFun (TC.tSeq (TC.tNum n) TC.tBit) (E.tValTy out)
  }

toSimpleType :: TC.Schema -> ModuleM SimpleType
toSimpleType schema = do
  env <- getEvalEnv
  case schema of
    TC.Forall [] _ ty -> case evalType env ty of
      (E.isTFun -> Just (E.isTSeq -> Just (E.numTValue -> Nat n, E.isTBit -> True), out)) -> do
        let simpleTy = SimpleType n out
            prettyTy = pretty (fromSimpleType simpleTy)
        case tvalueToType out of
          Right (Right (P.Forall [] [] pty _)) -> simpleTy <$ ensureCmp pty
          Left parseError -> fail ("Bug: couldn't parse " ++ prettyTy ++ " as a schema")
          Right (Left n)  -> fail ("Wasn't expecting to see a name in a monomorphic type value, but saw " ++ pretty n)
          Right (Right s) -> fail ("Bug: monomorphic type " ++ prettyTy ++ " was parsed as polymorphic")
      _ -> fail ("unsupported type " ++ pretty ty)
    _ -> fail "polymorphic types are unsupported"

deriving instance Foldable    P.Located
deriving instance Foldable    P.Prop
deriving instance Foldable    P.Schema
deriving instance Foldable    P.TParam
deriving instance Foldable    P.Type
deriving instance Functor     P.Prop
deriving instance Functor     P.Schema
deriving instance Functor     P.TParam
deriving instance Functor     P.Type
deriving instance Traversable P.Located
deriving instance Traversable P.Prop
deriving instance Traversable P.Schema
deriving instance Traversable P.TParam
deriving instance Traversable P.Type

tvalueToType :: E.TValue -> Either ParseError (Either P.PName (P.Schema a))
tvalueToType
  = fmap (traverse Left)
  . parseSchema
  . fromString
  . pretty
  . E.tValTy

ensureCmp :: P.Type TC.Name -> ModuleM ()
ensureCmp pty = do
  pm <- getPrimMap
  let eq = P.EVar (lookupPrimDecl (packIdent "==") pm) `P.ETyped`
         (pty `P.TFun` (pty `P.TFun` P.TBit))
  () <$ typeCheckInteractive eq

data ExprBuilderParams = ExprBuilderParams
  { freshVar     :: TC.Name
  , cryptolTrue  :: TC.Expr
  , cryptolFalse :: TC.Expr
  , cryptolCat   :: TC.Expr
  , cryptolNeq   :: TC.Expr
  }

getExprBuilderParams :: ModuleM ExprBuilderParams
getExprBuilderParams = do
    -- TODO: this is a terrible hack; all we really want is a fresh name and
    -- there has to be a better way to get one
    (TC.EAbs x _ _, _) <- checkExpr cryptolId
    pm <- getPrimMap
    let [true, false, cat, neq] = TC.ePrim pm . packIdent <$> ["True", "False", "#", "!="]
    return (ExprBuilderParams x true false cat neq)

cryptolId :: P.Expr P.PName
cryptolId = P.EFun [P.PVar (P.Located emptyRange (ident "x"))] (P.ETyped (evar "x") P.TBit)

step :: Integer -> [Bool] -> Maybe (Bool -> [Bool])
step n xs | genericLength xs < n = Just (\x -> xs ++ [x])
          | otherwise            = Nothing

checkEquality :: ExprBuilderParams -> String -> (TC.Expr, SimpleType) -> [Bool] -> [Bool] -> ModuleM Bool
checkEquality params solver unapplied l r = do
  let (expr, simpleTy) = equalityCondition params unapplied l r
  res <- satProve ProverCommand
    { pcQueryType  = SatQuery (SomeSat 1)
    , pcProverName = solver
    , pcVerbose    = False
    , pcExtraDecls = []
    , pcSmtFile    = Nothing
    , pcExpr       = expr
    , pcSchema     = fromSimpleType simpleTy
    }
  case res of
    ThmResult    _ -> return True
    AllSatResult _ -> return False
    ProverError  e -> fail e
    _              -> fail "SAT solver did something weird"

-- (!) assumes `length l == length r`
-- (!) assumes `length l <= n`
equalityCondition :: ExprBuilderParams -> (TC.Expr, SimpleType) -> [Bool] -> [Bool] -> (TC.Expr, SimpleType)
equalityCondition params (unapplied, SimpleType nin out) l r
  = assert (nl == nr && nl <= nin)
  $ (abstraction, SimpleType nfresh tvBit)
  where
  nl, nr, nfresh :: Integer
  nl = genericLength l
  nr = genericLength r
  nfresh = nin - nl

  infixl 1 $$, $^
  ($$) = TC.EApp
  ($^) = TC.ETApp
  tvBit          = E.TValue TC.tBit -- TODO: push this upstream
  liftBool True  = cryptolTrue params
  liftBool False = cryptolFalse params
  lift        bs = TC.EList (map liftBool bs) TC.tBit
  partialApp  bs = unapplied $$ (  cryptolCat params
                                $^ TC.tNum nl
                                $^ TC.tNum nfresh
                                $^ TC.tBit
                                $$ lift bs
                                $$ TC.EVar (freshVar params)
                                )
  abstraction    = TC.EAbs (freshVar params) (TC.tSeq (TC.tNum nfresh) TC.tBit)
                 $ cryptolNeq params $^ E.tValTy out $$ partialApp l $$ partialApp r

ident :: String -> P.PName
ident = P.UnQual . packIdent

evar :: String -> P.Expr P.PName
evar = P.EVar . ident
