module Convert.LDAG.DOT (convert) where

import Control.Lens ((^.), (<.>), (<.), each, itoListOf)
import Data.Graph.Inductive.Graph (Graph, mkGraph)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.GraphViz (GraphvizParams(clusterBy, clusterID, fmtCluster, fmtNode, fmtEdge, isDotCluster),
                      GlobalAttributes(GraphAttrs), GraphID(Num), NodeCluster(N, C), Number(Int),
                      dashed, graphToDot, defaultParams, printDotGraph, rounded, style, toLabel)
import Data.LDAG (LDAG, LayerID, allNodes, allLayers, nodeLabel, outgoing, dead)
import Data.List (group)
import Data.String (fromString)
import Data.Text.Lazy (Text)
import Data.Universe.Class (Finite, universeF)
import qualified Data.IntMap as IM

-- | (!) assumes that the nodes at different layers have different IDs
convert :: LDAG [Bool] Bool -> [String] -> Text
convert ldag grouping
  = printDotGraph
  . graphToDot (showParams grouping)
  . toFGLGr
  $ ldag

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

showParams :: [String] -> GraphvizParams n (LayerID, Bool, [Bool]) Bool (Int, String) (Bool, [Bool])
showParams grouping = defaultParams
  { clusterBy    = \(n, (layerID, dead, bs)) -> C (indexOf layerID) (N (n, (dead, bs)))
  , isDotCluster = \_ -> True
  , clusterID    = \(n, _) -> Num (Int n)
  , fmtCluster   = \(_, pos) -> [GraphAttrs [style rounded, toLabel pos]]
  , fmtNode      = \(_, (dead, bs)) -> [style dashed | dead]
                                    ++ [toLabel . showBools $ bs]
  , fmtEdge      = \(_, _, el)      -> [toLabel . showBool  $ el]
  } where
  indexOf layerID = groups !! max 0 (layerID-1)
  groups = concatMap (\(uniq, vals) -> map ((,) uniq) vals)
         . zip [0..]
         . group
         . cycle
         $ grouping

showBools = concatMap showBool
showBool False = "0"
showBool True  = "1"
