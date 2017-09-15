{-# LANGUAGE OverloadedStrings #-}
module Convert.LDAG.JSON (convert) where

import Control.Lens ((^.), (^..), (<.), _2, asIndex, at, filtered, itraversed, to)
import Convert.Misc.String (NodeLabel, showBools, showNodeLabel)
import Data.Aeson (Value, (.=), object, toJSON)
import Data.Aeson.Text (encodeToTextBuilder)
import Data.Aeson.Types (Pair)
import Data.Function (on)
import Data.LDAG
import Data.List (groupBy, nub)
import Data.Maybe (catMaybes)
import Data.String (fromString)
import Data.Text.Lazy (Text)
import Data.Text.Lazy.Builder (toLazyText)
import Data.Universe.Class (Finite(universeF))

import qualified Data.IntMap as IM
import qualified Data.Map    as M

-- | (!) assumes there is at least one layer
convert :: LDAG NodeLabel Bool -> [String] -> Text
convert ldag grouping
  = toLazyText . encodeToTextBuilder
  $ ldagGroupingToValue ldag grouping

ldagGroupingToValue ldag grouping = object
  [ "steps"   .= stepsToValue (ldagGroupingToSteps ldag grouping)
  , "outputs" .= [outputs ldag]
  ]

ldagGroupingToSteps :: (Eq pos, Finite e, Ord e)
  => LDAG n e -> [pos] -> [(pos, [([e], [[Bool]])])]
ldagGroupingToSteps ldag grouping
  = findAllPaths
  . map (\pairs@((_, label):_) -> (label, map fst pairs))
  . groupBy ((==) `on` snd)
  $ zip (ldag ^.. allLayers) (cycle grouping)

-- (!) assumes each grouping has at least one layer
findAllPaths ((label, layers):groups@((_, layer':_):_))
  = (label, findPathsTo (nodeIDs layer') layers) : findAllPaths groups
findAllPaths [(label, layers@(_:_:_))]
  = [(label, findPathsTo (nodeIDs (last layers)) (init layers))]
findAllPaths _ = []

findPathsTo :: (Finite e, Ord e) => [NodeID] -> [Layer n e] -> [([e], [[Bool]])]
findPathsTo endNodeIDs layers =
  filter nonZero
  [ (path, [ [ maybeEndNodeID == Just endNodeID
             | endNodeID <- endNodeIDs
             ]
           | startNodeID <- nodeIDs (head layers)
           , let maybeEndNodeID = transition startNodeID path layers
           ]
    )
  | path <- mapM (const universeF) layers
  ]
  where
  nonZero = or . map or . snd

nodeIDs :: Layer n e -> [NodeID]
nodeIDs layer = layer ^.. (allNodes <. dead . filtered not) . asIndex

transition :: Ord e => NodeID -> [e] -> [Layer n e] -> Maybe NodeID
transition nodeID [] [] = Just nodeID
transition nodeID (e:es) (layer:layers) = do
  node     <- layer ^. at nodeID
  children <- node  ^. outgoing
  transition (children e) es layers
transition nodeID _ _ = Nothing

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

outputs :: LDAG NodeLabel e -> [String]
outputs ldag = ldag ^.. layers . to IM.findMax . _2 . allNodes . filtered (not . (^. dead)) . nodeLabel . to showNodeLabel
