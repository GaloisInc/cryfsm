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

import Control.Monad.Loops (firstM)
import Control.Monad.State (MonadState, StateT, execStateT, get, gets, lift, modify, put)
import Control.Monad.Supply (evalSupplyT, supply)
import Data.Default (Default, def)
import Data.IntMap (IntMap)
import Data.Universe.Class (Finite, universeF)
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

unfoldLDAGM
  :: (Monad m, Finite e, Ord e)
  => (n -> n -> m Bool) -> (n -> Maybe (e -> n)) -> n -> m (LDAG n e)
unfoldLDAGM eq step = flip evalSupplyT universeF . flip execStateT def . go 0 where
  liftedEq a b = lift (lift (eq a b))
  go layerID n = do
    layer  <- getLayer layerID
    cached <- firstM (liftedEq n . nodeLabel . snd) (IM.assocs layer)
    case cached of
      Just (nodeID, _) -> return nodeID
      Nothing -> do
        nodeID   <- supply
        children <- traverse (\f -> sequenceA $ go (layerID+1) . f) (step n)
        modify (\(LDAG im) -> LDAG (IM.insert layerID (Layer (IM.insert nodeID (Node n children) layer)) im))
        return nodeID
