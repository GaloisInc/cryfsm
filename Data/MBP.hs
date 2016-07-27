{-# LANGUAGE OverloadedStrings #-}
module Data.MBP
  ( Symbol, Output, Position, Dimensions
  , MBP, Step, Matrix
  , mbp, step, matrix
  , steps, outputs, branches, position, elems
  , HasDimensions(..)
  ) where

import Control.Monad
import Data.Aeson
import Data.HashMap.Strict (HashMap)
import Data.Map (Map)
import Data.Maybe
import Data.Monoid
import Data.Text (Text)
import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T
import qualified Data.Map            as M

type Symbol     = Text
type Output     = Text
type Position   = Text
type Dimensions = (Int, Int)

-- constraints:
-- * steps is nonempty
-- * each step's column count is the next step's row count
-- * outputs has as many rows as the first step and as many columns as the last step
data MBP = MBP
  { steps   :: [Step]
  , outputs :: Matrix Output
  } deriving (Eq, Ord, Read, Show)

mbp :: [Step] -> Matrix Output -> Maybe MBP
mbp steps output = do
  outputDims <- checkDims (dims <$> steps)
  guard (outputDims == dims output)
  return (MBP steps output)
  where
  checkDims ((r,c):(r',c'):rest) = guard (c == r') >> checkDims ((r,c'):rest)
  checkDims short = listToMaybe short

-- constraints:
-- * at least one matrix
-- * all matrices have the same dimensions
-- * does not branch on "position" -- this is semantically stupid, and a
--   wart left over from bad JSON format design
data Step = Step
  { sDims    :: Dimensions
  , position :: Position
  , branches :: Map Symbol (Matrix Integer)
  } deriving (Eq, Ord, Read, Show)

step :: Position -> Map Symbol (Matrix Integer) -> Maybe Step
step position branches = do
  guard ("position" `M.notMember` branches)
  m:ms <- return (M.elems branches)
  guard (all (\m' -> dims m == dims m') ms)
  return (Step (dims m) position branches)

-- constraints:
-- * rows > 0
-- * cols > 0
-- * map length elems = replicate rows cols
data Matrix field = Matrix
  { mDims :: Dimensions
  , elems :: [[field]]
  } deriving (Eq, Ord, Read, Show)

matrix :: [[field]] -> Maybe (Matrix field)
matrix elems = do
  e:es <- return elems
  let cols = length e
      rows = length elems
  guard (all (\e' -> cols == length e') es)
  return (Matrix (rows, cols) elems)

class    HasDimensions a          where dims :: a -> Dimensions
instance HasDimensions Step       where dims = sDims
instance HasDimensions (Matrix a) where dims = mDims

instance ToJSON field => ToJSON (Matrix field) where
  toJSON     = toJSON     . elems
  toEncoding = toEncoding . elems

instance FromJSON field => FromJSON (Matrix field) where
  parseJSON v = do
    elems <- parseJSON v
    let rows = length elems
        cols:colss = map length elems
    if rows > 0 && all (cols==) colss
       then return (Matrix (rows, cols) elems)
       else fail "expected a non-empty rectangular array of arrays"

instance ToJSON Step where
  toJSON (Step _ position branches) = toJSON (M.insert "position" (toJSON position) (toJSON <$> branches))
  toEncoding (Step _ position branches)
    = pairs
    $  "position" .= position
    <> foldMap (uncurry (.=)) (M.toList branches)

instance FromJSON Step where
  parseJSON = withObject "step" $ \o -> do
    position <- o .: "position"
    branches <- parseJSON (Object (HM.delete "position" o))
    case M.elems branches of
      [] -> fail "each step must have at least one branch"
      b:bs | all (\b' -> dims b == dims b') bs -> return (Step (dims b) position branches)
           | otherwise -> fail "all branches in the step must have the same dimensions"

instance ToJSON MBP where
  toJSON     (MBP steps outputs) = object ["steps" .= steps,   "outputs" .= outputs]
  toEncoding (MBP steps outputs) = pairs  ("steps" .= steps <> "outputs" .= outputs)

instance FromJSON MBP where
  parseJSON = withObject "matrix branching program" $ \o -> do
    steps   <- o .: "steps"
    when (null steps) (fail "program must have at least one step")
    outputs <- o .: "outputs"
    return (MBP steps outputs)
