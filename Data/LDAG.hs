{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE Rank2Types #-}
module Data.LDAG
  ( LDAG, layers, allLayers
  , Layer, nodes, allNodes
  , Node, nodeLabel, outgoing
  , LayerMap
  , NodeMap
  , LayerID
  , NodeID
  , unfoldLDAGM
  ) where

import Control.Lens (At(at), Ixed(ix), Index, IndexedTraversal, IxValue, Lens', (?=), anon, itoListOf, itraversed, makeLenses, to, use)
import Control.Monad.Loops (firstM)
import Control.Monad.State (MonadState, StateT, execStateT, get, gets, lift, modify, put)
import Control.Monad.Supply (evalSupplyT, supply)
import Data.Default (Default, def)
import Data.IntMap (IntMap)
import Data.Map (Map)
import Data.Universe.Instances.Base (universeF)

import qualified Data.IntMap as IM
import qualified Data.Map    as M

type NodeID   = Int
type LayerID  = Int
type NodeMap  = IntMap
type LayerMap = IntMap

data Node n e = Node
  { _nodeLabel :: n
  , _outgoing  :: Map e NodeID
  } deriving (Eq, Ord, Read, Show)

newtype Layer n e = Layer { _nodes :: NodeMap (Node n e) }
  deriving (Eq, Ord, Read, Show, Default)

newtype LDAG n e = LDAG { _layers :: LayerMap (Layer n e) }
  deriving (Eq, Ord, Read, Show, Default)

makeLenses ''Node
makeLenses ''Layer
makeLenses ''LDAG

allLayers :: IndexedTraversal LayerID (LDAG  n e) (LDAG  n' e') (Layer n e) (Layer n' e')
allNodes  :: IndexedTraversal NodeID  (Layer n e) (Layer n' e') (Node  n e) (Node  n' e')
allLayers = layers . itraversed
allNodes  = nodes  . itraversed

type instance Index   (LDAG  n e) = LayerID
type instance Index   (Layer n e) = NodeID
type instance IxValue (LDAG  n e) = Layer n e
type instance IxValue (Layer n e) = Node  n e
instance Ixed (LDAG  n e) where ix i = layers . ix i
instance Ixed (Layer n e) where ix i = nodes  . ix i
instance At   (LDAG  n e) where at i = layers . at i
instance At   (Layer n e) where at i = nodes  . at i

atLayer :: LayerID -> Lens' (LDAG n e) (Layer n e)
atLayer id = at id . anon def (null . _nodes)

unfoldLDAGM
  :: (Monad m, Ord e)
  => (n -> n -> m Bool) -> (n -> m (Map e n)) -> n -> m (LDAG n e)
unfoldLDAGM eq step = flip evalSupplyT universeF . flip execStateT def . go 0 where
  lift2 = lift . lift
  liftedEq a b = lift2 (eq a b)
  go layerID n = do
    nodeMap <- gets . itoListOf $ atLayer layerID . allNodes
    cached  <- firstM (liftedEq n . _nodeLabel . snd) nodeMap
    case cached of
      Just (nodeID, _) -> return nodeID
      Nothing -> do
        nodeID   <- supply
        children <- lift2 (step n) >>= traverse (go (layerID+1))
        atLayer layerID . at nodeID ?= Node n children
        return nodeID
