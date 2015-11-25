{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
import Control.Monad.Error.Class
import Control.Monad.Loops
import Control.Monad.State
import Cryptol.Eval.Value
import Cryptol.ModuleSystem
import Cryptol.ModuleSystem.Monad
import Cryptol.Parser.AST
import Cryptol.Parser.Name
import Cryptol.Parser.Position
import Cryptol.Symbolic
import Cryptol.TypeCheck.Solver.InfNat
import Cryptol.Utils.Ident
import Cryptol.Utils.PP
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
import System.Environment
import System.Exit

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

instance Monad m => MonadError ModuleError (ModuleT m) where
  throwError e = ModuleT (MonadLib.raise e)
  catchError m f = ModuleT (MonadLib.try (unModuleT m) >>= either (unModuleT . f) return)

instance MonadIO m => MonadIO (ModuleT m) where
  liftIO act = MonadLib.lift (liftIO act)

inputBits :: ModuleM Integer
inputBits = do
  (_, _, schema) <- liftToBase . checkExpr . EVar . UnQual . packIdent $ "main"
  env <- getEvalEnv
  case schema of
    TC.Forall [] _ ty -> case TC.evalType env ty of
      (isTFun -> Just (isTSeq -> Just (numTValue -> Nat n, isTBit -> True), _)) -> return n
      _ -> fail ("unsupported type " ++ pretty ty)
    _ -> fail "polymorphic types are unsupported"

step :: Integer -> [Bool] -> Maybe (Bool -> [Bool])
step n xs | genericLength xs < n = Just (\x -> xs ++ [x])
          | otherwise            = Nothing

checkEquality :: [Bool] -> [Bool] -> ModuleM Bool
checkEquality l r = do
  (_, expr, schema) <- liftToBase $ checkExpr (equalityCondition l r)
  res <- liftToBase $ satProve ProverCommand
    { pcQueryType  = SatQuery (SomeSat 1)
    , pcProverName = "cvc4"
    , pcVerbose    = False
    , pcExtraDecls = []
    , pcSmtFile    = Nothing
    , pcExpr       = expr
    , pcSchema     = schema
    }
  case res of
    ThmResult    _ -> return True
    AllSatResult _ -> return False
    _              -> fail "SAT solver did something weird"

equalityCondition :: [Bool] -> [Bool] -> Expr PName
equalityCondition l r = EFun [pat "y"]
  (ident "!=" $$ partialMain l $$ partialMain r)
  where
  infixl 1 $$
  pat   = PVar . Located emptyRange . UnQual . packIdent
  ident = EVar . UnQual . packIdent
  ($$)  = EApp
  lift  = EList . map (ident . show)
  partialMain bs = ident "main" $$ (ident "#" $$ lift bs $$ ident "y")

showBool False = "0"
showBool True  = "1"
showBools = concatMap showBool

showParams :: GraphvizParams n (LayerID, [Bool]) Bool () (LayerID, [Bool])
showParams = nonClusteredParams
  { fmtNode = \(_, (_, bs)) -> [toLabel . showBools $ bs]
  , fmtEdge = \(_, _, el)   -> [toLabel . showBool  $ el]
  }

main = do
  args <- getArgs
  let howToPrint = case args of
        [file] -> T.writeFile file
        _      -> T.putStrLn
  env <- initialModuleEnv
  runModuleM env $ do
    mod  <- liftToBase $ loadModuleByPath "main.cry"
    len  <- inputBits
    ldag <- evalStateT (unfoldLDAGM ((lift .) . checkEquality) (step len) []) 0
    io . howToPrint . printDotGraph . graphToDot showParams . toFGL $ ldag
