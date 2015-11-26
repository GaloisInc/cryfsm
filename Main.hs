{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Exception
import Control.Monad.Error.Class
import Control.Monad.Loops
import Control.Monad.State
import Cryptol.Eval.Value
import Cryptol.ModuleSystem
import Cryptol.ModuleSystem.Monad
import Cryptol.Parser
import Cryptol.Parser.AST
import Cryptol.Parser.Name
import Cryptol.Parser.Position
import Cryptol.Symbolic
import Cryptol.TypeCheck.Solver.InfNat
import Cryptol.Utils.Ident
import Cryptol.Utils.PP (pretty)
import Data.Default
import Data.IntMap (IntMap)
import Data.Graph.Inductive.Graph (Graph, mkGraph)
import Data.Graph.Inductive.PatriciaTree
import Data.GraphViz
import Data.GraphViz.Types
import Data.List
import Data.String
import Data.Universe.Class
import Data.Universe.Instances.Eq
import Data.Universe.Instances.Ord
import Data.Universe.Instances.Read
import Data.Universe.Instances.Show
import Data.Universe.Instances.Traversable
import Options.Applicative
import System.Environment
import System.Exit
import System.IO

import qualified Cryptol.TypeCheck.AST as TC
import qualified Cryptol.Eval.Type     as TC
import qualified Data.IntMap           as IM
import qualified Data.Text.Lazy.IO     as T
import qualified MonadLib

type NodeID   = Int
type LayerID  = Int
type NodeMap  = IntMap
type LayerMap = IntMap

data Node n e = Node
  { nodeLabel :: n
  , outgoing  :: Maybe (e -> NodeID)
  } deriving (Eq, Ord, Read, Show)

newtype Layer n e = Layer { nodes :: NodeMap (Node n e) }
  deriving (Eq, Ord, Read, Show, Default)

newtype LDAG n e = LDAG { layers :: LayerMap (Layer n e) }
  deriving (Eq, Ord, Read, Show, Default)

-- (!) assumes that the nodes at different layers have different IDs
-- toFGL :: (Graph g, Finite e) => LDAG n e -> g (Int, n) e
toFGL :: LDAG [Bool] Bool -> Gr (Int, [Bool]) Bool
toFGL ldag = mkGraph ns es where
  ns = [ (nodeID, (layerID, nodeLabel node))
       | (layerID, layer) <- IM.assocs (layers ldag)
       , (nodeID, node)   <- IM.assocs (nodes layer)
       ]
  es = [ (fromNodeID, children edgeLabel, edgeLabel)
       | layer <- IM.elems (layers ldag)
       , (fromNodeID, Node { outgoing = Just children })
           <- IM.assocs (nodes layer)
       , edgeLabel <- universeF
       ]

getLayer :: MonadState (LDAG n e) m => LayerID -> m (NodeMap (Node n e))
getLayer layerID = gets (nodes . IM.findWithDefault def layerID . layers)

getNodeID :: MonadState NodeID m => m NodeID
getNodeID = do
  nodeID <- get
  put (nodeID + 1)
  return nodeID

try :: MonadError e m => m a -> m (Maybe a)
try act = (Just <$> act) `catchError` \_ -> return Nothing

unfoldLDAGM
  :: forall m err n e. (MonadState NodeID m, Finite e, Ord e)
  => (n -> n -> m Bool) -> (n -> Maybe (e -> n)) -> n -> m (LDAG n e)
unfoldLDAGM eq step = flip execStateT def . go 0 where
  go :: LayerID -> n -> StateT (LDAG n e) m NodeID
  go layerID n = do
    layer  <- getLayer layerID
    cached <- firstM (\(nodeID, node) -> lift (eq (nodeLabel node) n)) (IM.assocs layer)
    case cached of
      Just (nodeID, _) -> return nodeID
      Nothing -> do
        nodeID   <- lift getNodeID
        children <- traverse (\f -> sequenceA $ go (layerID+1) . f) (step n)
        modify (\(LDAG im) -> LDAG (IM.insert layerID (Layer (IM.insert nodeID (Node n children) layer)) im))
        return nodeID

liftToBase :: ModuleCmd a -> ModuleM a
liftToBase f = ModuleT $ do
  env <- MonadLib.get
  (res, ws) <- MonadLib.inBase (f env)
  MonadLib.put ws
  case res of
    Left err -> MonadLib.raise err
    Right (val, env') -> val <$ MonadLib.set env'

checkExprBase :: Expr PName -> ModuleM (TC.Expr, TC.Schema)
checkExprBase parsed = (\(_, e, s) -> (e, s)) <$> liftToBase (checkExpr parsed)

data SimpleType = SimpleType
  { inputBits  :: Integer
  , outputType :: TValue
  }

fromSimpleType :: SimpleType -> TC.Schema
fromSimpleType (SimpleType n out) = TC.Forall
  { TC.sVars  = []
  , TC.sProps = []
  , TC.sType  = TC.tFun (TC.tSeq (TC.tNum n) TC.tBit) (tValTy out)
  }

toSimpleType :: TC.Schema -> ModuleM SimpleType
toSimpleType schema = do
  env <- getEvalEnv
  case schema of
    TC.Forall [] _ ty -> case TC.evalType env ty of
      (isTFun -> Just (isTSeq -> Just (numTValue -> Nat n, isTBit -> True), out)) -> return (SimpleType n out)
      _ -> fail ("unsupported type " ++ pretty ty)
    _ -> fail "polymorphic types are unsupported"

step :: Integer -> [Bool] -> Maybe (Bool -> [Bool])
step n xs | genericLength xs < n = Just (\x -> xs ++ [x])
          | otherwise            = Nothing

checkEquality :: ExprBuilderParams -> (TC.Expr, SimpleType) -> [Bool] -> [Bool] -> ModuleM Bool
checkEquality params unapplied l r = do
  let (expr, simpleTy) = equalityCondition params unapplied l r
  res <- liftToBase $ satProve ProverCommand
    { pcQueryType  = SatQuery (SomeSat 1)
    , pcProverName = "cvc4"
    , pcVerbose    = False
    , pcExtraDecls = []
    , pcSmtFile    = Nothing
    , pcExpr       = expr
    , pcSchema     = fromSimpleType simpleTy
    }
  case res of
    ThmResult    _ -> return True
    AllSatResult _ -> return False
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
  tvBit          = TValue TC.tBit -- TODO: push this upstream
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
                 $ cryptolNeq params $^ tValTy out $$ partialApp l $$ partialApp r

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
    (TC.EAbs x _ _, _) <- checkExprBase cryptolId
    (true         , _) <- checkExprBase $ EVar (ident "True" )
    (false        , _) <- checkExprBase $ EVar (ident "False")
    (cat          , _) <- checkExprBase $ EVar (ident "#"    )
    (neq          , _) <- checkExprBase $ EVar (ident "!="   )
    return (ExprBuilderParams x true false cat neq)

showBool False = "0"
showBool True  = "1"
showBools = concatMap showBool

showParams :: GraphvizParams n (LayerID, [Bool]) Bool () (LayerID, [Bool])
showParams = nonClusteredParams
  { fmtNode = \(_, (_, bs)) -> [toLabel . showBools $ bs]
  , fmtEdge = \(_, _, el)   -> [toLabel . showBool  $ el]
  }

data OutputFormat = DOT deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Options = Options
  { optOutputPath   :: Maybe FilePath
  , optExpr         :: Expr PName
  , optOutputFormat :: OutputFormat
  , optSolver       :: String
  , optModules      :: [FilePath]
  } deriving (Eq, Show)

knownSolvers :: [String]
knownSolvers = map fst proverConfigs

knownSolversString = intercalate ", " knownSolvers

knownSolverParser :: ReadM String
knownSolverParser = do
  s <- str
  unless (s `elem` knownSolvers) (readerError $  "unknown solver "
                                              ++ s
                                              ++ "; choose from "
                                              ++ knownSolversString
                                 )
  return s

exprParser :: ReadM (Expr PName)
exprParser = do
  s <- str
  case parseExpr (fromString s) of
    Left err -> readerError $ "couldn't parse cryptol expression\n" ++ pretty err
    Right v  -> return v

options_ :: Parser Options
options_ = Options
  <$> optional (strOption (  short 'o'
                          <> metavar "FILE"
                          <> help "output file (default stdin)"
                          )
               )
  <*> (option exprParser (  short 'e'
                         <> metavar "EXPR"
                         <> help "a cryptol expression to partially evaluate (default main)"
                         )
      <|> pure (EVar . UnQual . packIdent $ "main")
      )
  <*> (option auto (  short 'f'
                   <> metavar "FORMAT"
                   <> help (  "output format: "
                           ++ intercalate ", " (map show [minBound .. maxBound :: OutputFormat])
                           ++ " (default DOT)"
                           )
                   )
      <|> pure DOT
      )
  <*> (option knownSolverParser (  short 's'
                                <> metavar "SOLVER"
                                <> help (  "which SMT solver to use: "
                                        ++ knownSolversString
                                        ++ " (default cvc4)"
                                        )
                                )
      <|> pure "cvc4"
      )
  <*> many (argument str (metavar "FILE ..."))

options :: ParserInfo Options
options = info (helper <*> options_)
  (  fullDesc
  <> progDesc "Produce a finite state machine that emulates EXPR, inspecting one input bit at a time, using SOLVER to check equality of states. The type of EXPR should be monomorphic, and unify with `{a} (Cmp a) => [n] -> a`."
  )

ident :: String -> PName
ident = UnQual . packIdent

cryptolId :: Expr PName
cryptolId = EFun [PVar (Located emptyRange (ident "x"))] (ETyped (EVar (ident "x")) TBit)

main = do
  opts <- execParser options
  let howToPrint = case optOutputPath opts of
        Just file -> T.writeFile file
        _         -> T.putStrLn
  env <- initialModuleEnv
  res <- runModuleM env $ do
    mapM_ (liftToBase . loadModuleByPath) (optModules opts)
    (expr, schema) <- checkExprBase $ optExpr opts
    simpleTy <- toSimpleType schema
    params   <- getExprBuilderParams
    ldag     <- evalStateT (unfoldLDAGM ((lift .) . checkEquality params (expr, simpleTy)) (step (inputBits simpleTy)) []) 0
    io . howToPrint . printDotGraph . graphToDot showParams . toFGL $ ldag
  exitCode <- case res of
    (Left err, _ ) -> ExitFailure 1 <$ hPutStrLn stderr (pretty err)
    (_       , ws) -> ExitSuccess   <$ mapM_ (putStrLn . pretty) ws
  exitWith exitCode
