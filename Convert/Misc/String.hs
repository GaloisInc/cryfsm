module Convert.Misc.String
  ( NodeLabel
  , showNodeLabel
  , showValue
  , showBools
  , showBool
  ) where

import Cryptol.Utils.PP (pretty)
import Cryptol.Eval.Value (Value, WithBase(WithBase), defaultPPOpts, useAscii)

type NodeLabel = Either [Bool] Value

showNodeLabel :: NodeLabel -> String
showNodeLabel = either showBools showValue

showValue :: Value -> String
showValue = pretty . WithBase defaultPPOpts { useAscii = True }

showBools :: [Bool] -> String
showBools = concatMap showBool

showBool :: Bool -> String
showBool False = "0"
showBool True  = "1"
