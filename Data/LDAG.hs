{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.LDAG
  ( LDAG(..)
  , Layer(..)
  , Node(..)
  , LayerMap
  , NodeMap
  , LayerID
  , NodeID
  , unfoldLDAGM
  ) where

import Control.Monad.State (MonadState, StateT, execStateT, get, gets, lift, modify, put)
import Control.Monad.Loops (firstM)
import Data.Default (Default, def)
import Data.IntMap (IntMap)
import Data.Universe.Class (Finite)
import Data.Universe.Instances.Eq ()
import Data.Universe.Instances.Ord ()
import Data.Universe.Instances.Read ()
import Data.Universe.Instances.Show ()
import Data.Universe.Instances.Traversable ()

import qualified Data.IntMap as IM

type NodeID   = Int
type LayerID  = Int
type NodeMap  = IntMap
type LayerMap = IntMap

data Node n e = Node
  { nodeLabel :: n
  -- TODO: Compose?
  , outgoing  :: Maybe (e -> NodeID)
  } deriving (Eq, Ord, Read, Show)

newtype Layer n e = Layer { nodes :: NodeMap (Node n e) }
  deriving (Eq, Ord, Read, Show, Default)

newtype LDAG n e = LDAG { layers :: LayerMap (Layer n e) }
  deriving (Eq, Ord, Read, Show, Default)

getLayer :: MonadState (LDAG n e) m => LayerID -> m (NodeMap (Node n e))
getLayer layerID = gets (nodes . IM.findWithDefault def layerID . layers)

getNodeID :: MonadState NodeID m => m NodeID
getNodeID = do
  nodeID <- get
  put (nodeID + 1)
  return nodeID

-- TODO: this interface is ridiculous; why do we demand `MonadState NodeID m`
-- instead of just `Monad m` and wrapping with `StateT` ourselves?
unfoldLDAGM
  :: (MonadState NodeID m, Finite e, Ord e)
  => (n -> n -> m Bool) -> (n -> Maybe (e -> n)) -> n -> m (LDAG n e)
unfoldLDAGM eq step = flip execStateT def . go 0 where
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
