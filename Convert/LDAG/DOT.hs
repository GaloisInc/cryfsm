module Convert.LDAG.DOT (convert) where

import Control.Lens ((^.), (<.>), (<.), each, itoListOf)
import Data.Graph.Inductive.Graph (Graph, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (GraphvizParams(fmtNode, fmtEdge), dashed, graphToDot, nonClusteredParams, printDotGraph, style, toLabel)
import Data.LDAG (LDAG, LayerID, allNodes, allLayers, nodeLabel, outgoing, dead)
import Data.Text.Lazy (Text)
import Data.Universe.Class (Finite, universeF)
import qualified Data.IntMap as IM

-- | (!) assumes that the nodes at different layers have different IDs
convert :: LDAG [Bool] Bool -> Text
convert = printDotGraph . graphToDot showParams . toFGLGr

toFGLGr :: Finite e => LDAG n e -> Gr (LayerID, Bool, n) e
toFGLGr = toFGL

toFGL :: (Graph gr, Finite e) => LDAG n e -> gr (LayerID, Bool, n) e
toFGL ldag = mkGraph ns es where
  ns = [ (nodeID, (layerID, node ^. dead, node ^. nodeLabel))
       | ((layerID, nodeID), node)
           <- itoListOf (allLayers <.> allNodes) ldag
       ]
  es = [ (fromNodeID, children edgeLabel, edgeLabel)
       | (fromNodeID, children)
           <- itoListOf (allLayers . allNodes <. outgoing . each) ldag
       , edgeLabel <- universeF
       ]

showParams :: GraphvizParams n (LayerID, Bool, [Bool]) Bool () (LayerID, Bool, [Bool])
showParams = nonClusteredParams
  { fmtNode = \(_, (_, dead, bs)) -> [style dashed | dead]
                                  ++ [toLabel . showBools $ bs]
  , fmtEdge = \(_, _, el)         -> [toLabel . showBool  $ el]
  }

showBools = concatMap showBool
showBool False = "0"
showBool True  = "1"
