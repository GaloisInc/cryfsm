{-# LANGUAGE ViewPatterns #-}
module Cryptol.FSM
  ( SimpleType(..)
  , fromSimpleType
  , toSimpleType
  , checkExprSimpleType
  , ExprBuilderParams
  , getExprBuilderParams
  , step
  , checkEquality
  , validityAscription
  ) where

import Control.Exception (assert)
import Cryptol.Eval.Type (evalType)
import Cryptol.ModuleSystem.Name (lookupPrimDecl)
import Cryptol.ModuleSystem.Renamer (rename)
import Cryptol.Parser (ParseError, parseSchema)
import Cryptol.Parser.Position (emptyRange)
import Cryptol.Symbolic (ProverCommand(ProverCommand), ProverResult(AllSatResult, ThmResult, ProverError), QueryType(SatQuery), SatNum(SomeSat), pcExpr, pcExtraDecls, pcProverName, pcQueryType, pcSchema, pcSmtFile, pcVerbose)
import Cryptol.TypeCheck.Solver.InfNat (Nat'(Nat))
import Cryptol.Utils.Ident (packIdent)
import Cryptol.ModuleM (ModuleM, checkExpr, getEvalEnv, getPrimMap, satProve, renameInteractive, typeCheckInteractive)
import Cryptol.Utils.PP (pretty)
import Data.List (genericLength)
import Data.Map (Map)
import Data.String (fromString)
import Data.Universe.Instances.Base (universeF)
import qualified Cryptol.Eval.Value    as E
import qualified Cryptol.Parser.AST    as P
import qualified Cryptol.TypeCheck.AST as TC
import qualified Data.Map              as M

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
        schema <- tvalueToSchema out
        case schema of
          Right (P.Forall [] [] pty _) -> simpleTy <$ ensureCmp pty
          Left  parseError -> fail ("Bug: couldn't parse " ++ prettyTy ++ " as a schema")
          Right s          -> fail ("Bug: monomorphic type " ++ prettyTy ++ " was parsed as polymorphic")
      _ -> fail ("unsupported type " ++ pretty ty)
    _ -> fail "polymorphic types are unsupported"

checkExprSimpleType :: P.Expr P.PName -> ModuleM (TC.Expr, SimpleType)
checkExprSimpleType e = checkExpr e >>= traverse toSimpleType

tvalueToSchema :: E.TValue -> ModuleM (Either ParseError (P.Schema TC.Name))
tvalueToSchema
  = traverse (renameInteractive . rename)
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

sat :: String -> (TC.Expr, SimpleType) -> ModuleM Bool
sat solver (expr, simpleTy) = do
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
    ThmResult    _ -> return False
    AllSatResult _ -> return True
    ProverError  e -> fail e
    _              -> fail "SAT solver did something weird"

step :: ExprBuilderParams -> String -> Integer -> (TC.Expr, SimpleType) -> [Bool] -> ModuleM (Map Bool [Bool])
step params solver n valid xs
  | genericLength xs >= n = return M.empty
  | otherwise             = M.fromList . concat <$> mapM filterValid universeF
  where
  filterValid x = do
    let xs' = xs ++ [x]
    couldBeValid <- sat solver (validCondition params valid xs')
    return [(x, xs') | couldBeValid]

checkEquality :: ExprBuilderParams -> String -> (TC.Expr, SimpleType) -> [Bool] -> [Bool] -> ModuleM Bool
checkEquality params solver unapplied l r
  = fmap not
  . sat solver
  $ equalityCondition params unapplied l r

-- (!) assumes `length l == length r`
-- (!) assumes `length l <= nin`
equalityCondition :: ExprBuilderParams -> (TC.Expr, SimpleType) -> [Bool] -> [Bool] -> (TC.Expr, SimpleType)
equalityCondition params (unapplied, SimpleType nin out) l r
  = assert (nl == nr && nl <= nin)
  $ (abstraction, SimpleType nfresh tvBit)
  where
  nl, nr, nfresh :: Integer
  nl = genericLength l
  nr = genericLength r
  nfresh = nin - nl

  partialApp  bs = unapplied $$ (  cryptolCat params
                                $^ TC.tNum nl
                                $^ TC.tNum nfresh
                                $^ TC.tBit
                                $$ liftBools params bs
                                $$ TC.EVar (freshVar params)
                                )
  abstraction    = TC.EAbs (freshVar params) (TC.tSeq (TC.tNum nfresh) TC.tBit)
                 $ cryptolNeq params $^ E.tValTy out $$ partialApp l $$ partialApp r

-- (!) assums `length i <= nin`
validCondition :: ExprBuilderParams -> (TC.Expr, SimpleType) -> [Bool] -> (TC.Expr, SimpleType)
validCondition params (valid, SimpleType nin out) i
  = assert (ni <= nin)
  $ (abstraction, SimpleType nfresh tvBit)
  where
  ni, nfresh :: Integer
  ni     = genericLength i
  nfresh = nin - ni

  abstraction = TC.EAbs (freshVar params) (TC.tSeq (TC.tNum nfresh) TC.tBit)
              $ valid $$ (  cryptolCat params
                         $^ TC.tNum ni
                         $^ TC.tNum nfresh
                         $^ TC.tBit
                         $$ liftBools params i
                         $$ TC.EVar (freshVar params)
                         )

validityAscription :: Integer -> P.Expr P.PName -> P.Expr P.PName
validityAscription n e = P.ETyped e (P.TFun (P.TSeq (P.TNum n) P.TBit) P.TBit)

infixl 1 $$, $^
($$) :: TC.Expr -> TC.Expr -> TC.Expr
($^) :: TC.Expr -> TC.Type -> TC.Expr
($$) = TC.EApp
($^) = TC.ETApp

tvBit :: E.TValue
tvBit = E.TValue TC.tBit -- TODO: push this upstream

liftBool  :: ExprBuilderParams ->  Bool  -> TC.Expr
liftBools :: ExprBuilderParams -> [Bool] -> TC.Expr
liftBool  params True  = cryptolTrue params
liftBool  params False = cryptolFalse params
liftBools params bs    = TC.EList (liftBool params <$> bs) TC.tBit

ident :: String -> P.PName
ident = P.UnQual . packIdent

evar :: String -> P.Expr P.PName
evar = P.EVar . ident
