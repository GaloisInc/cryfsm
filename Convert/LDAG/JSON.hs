{-# LANGUAGE OverloadedStrings #-}
module Convert.LDAG.JSON (convert) where

import Control.Lens ((^.), (^..), _2, asIndex, at, itraversed, to)
import Data.Aeson (Value, (.=), object, toJSON)
import Data.Aeson.Encode (encodeToTextBuilder)
import Data.Aeson.Types (Pair)
import Data.Function (on)
import Data.LDAG
import Data.List (groupBy, nub)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)

import qualified Data.IntMap as IM
import qualified Data.Map    as M

-- | (!) assumes there is at least one layer
convert :: LDAG [Bool] Bool -> [String] -> Text
convert ldag grouping
  = toLazyText . encodeToTextBuilder
  $ ldagGroupingToValue ldag grouping

ldagGroupingToValue ldag grouping = object
  [ "steps"   .= stepsToValue (ldagGroupingToSteps ldag grouping)
  , "outputs" .= outputs ldag
  ]

ldagGroupingToSteps :: (Eq pos, Ord e) => LDAG n e -> [pos] -> [(pos, [([e], [[Bool]])])]
ldagGroupingToSteps ldag grouping
  = findAllPaths
  . map (\pairs@((_, label):_) -> (label, map fst pairs))
  . groupBy ((==) `on` snd)
  $ zip (ldag ^.. allLayers) (cycle grouping)

-- (!) assumes each grouping has at least one layer
findAllPaths ((label, layers):groups@((_, layer':_):_))
  = (label, findPathsTo (nodeIDs layer') layers) : findAllPaths groups
findAllPaths [(label, layers@(_:_))]
  = [(label, findPathsTo (nodeIDs (last layers)) (init layers))]
findAllPaths _ = []

findPathsTo :: Ord e => [NodeID] -> [Layer n e] -> [([e], [[Bool]])]
findPathsTo endNodeIDs layers =
  [ (path, [ [ transition startNode path layers == Just endNode
             | endNode <- endNodeIDs
             ]
           | startNode <- nodeIDs (head layers)
           ]
    )
  | path <- allPaths layers
  ]

allPaths :: Ord e => [Layer n e] -> [[e]]
allPaths [] = [[]]
allPaths layers@(layer:_) = go (layer ^.. allNodes . asIndex) layers where
  go [] _ = []
  go activeNodeIDs [] = [[]]
  go activeNodeIDs (layer:layers) = do
    edge <- edges
    path <- go (step edge) layers
    return (edge:path)
    where
    edges = case catMaybes [layer ^. at nodeID | nodeID <- activeNodeIDs] of
      []    -> [] -- should never happen
      nodes -> M.keys . foldr1 (M.intersection) . map (^. outgoing) $ nodes
    step edge = nub . catMaybes
              $ [transition nodeID [edge] [layer] | nodeID <- activeNodeIDs]

nodeIDs :: Layer n e -> [NodeID]
nodeIDs layer = layer ^.. allNodes . asIndex

transition :: Ord e => NodeID -> [e] -> [Layer n e] -> Maybe NodeID
transition nodeID [] [] = Just nodeID
transition nodeID (e:es) (layer:layers) = do
  node    <- layer ^. at nodeID
  nodeID' <- M.lookup e (node ^. outgoing)
  transition nodeID' es layers
transition nodeID _ _ = Nothing

allEdges :: Eq e => Layer n e -> [e]
allEdges layer = nub (layer ^.. allNodes . outgoing . itraversed . asIndex)

stepsToValue :: [(String, [([Bool], [[Bool]])])] -> Value
stepToValue  ::  (String, [([Bool], [[Bool]])])  -> Value
stepsToValue = toJSON . map stepToValue
stepToValue (position, mappings) = object
  $ "position" .= position
  : map mappingToPair mappings

mappingToPair :: ([Bool], [[Bool]]) -> Pair
mappingToPair (label, matrix) = k .= v where
  k = fromString (showBools label)
  v = map (map fromEnum) matrix

outputs :: LDAG [Bool] e -> [String]
outputs ldag = ldag ^.. layers . to IM.findMax . _2 . allNodes . nodeLabel . to showBools

showBools :: [Bool] -> String
showBools = concatMap showBool

showBool :: Bool -> String
showBool False = "0"
showBool True  = "1"
