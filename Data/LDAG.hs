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
  { _nodeLabel :: n
  , _outgoing  :: Maybe (e -> NodeID)
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
  :: (Monad m, Finite e, Ord e)
  => (n -> n -> m Bool) -> (n -> Maybe (e -> n)) -> n -> m (LDAG n e)
unfoldLDAGM eq step = flip evalSupplyT universeF . flip execStateT def . go 0 where
  liftedEq a b = lift (lift (eq a b))
  go layerID n = do
    nodeMap <- gets . itoListOf $ atLayer layerID . allNodes
    cached  <- firstM (liftedEq n . _nodeLabel . snd) nodeMap
    case cached of
      Just (nodeID, _) -> return nodeID
      Nothing -> do
        nodeID   <- supply
        children <- traverse (traverse (go (layerID+1))) (step n)
        atLayer layerID . at nodeID ?= Node n children
        return nodeID
