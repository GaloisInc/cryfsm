import Control.Monad.State (evalStateT, lift, unless, when)
import Convert.LDAG.DOT
import Cryptol.Eval.Value (isTBit)
import Cryptol.FSM
import Cryptol.ModuleM
import Cryptol.Utils.PP (pretty)
import Data.LDAG
import Options
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

import qualified Data.Text.Lazy.IO as T

main = do
  opts <- getOpts
  let howToPrint = case optOutputPath opts of
        Just file -> T.writeFile file
        _         -> T.putStrLn
  res <- runModuleM $ do
    when (null (optModules opts)) loadPrelude
    mapM_ loadModuleByPath (optModules opts)
    function <-          checkExprSimpleType $ optFunction opts
    valid    <- traverse checkExprSimpleType $ optValid    opts
    unless (maybe True (isTBit . outputType . snd) valid) $
      fail "validity-checking expression must output `Bit`"
    params <- getExprBuilderParams
    ldag   <- unfoldLDAGM (checkEquality params (optSolver opts) function)
                          (step params (optSolver opts) (inputBits (snd function)) valid)
                          []
    io . howToPrint . convert $ ldag
  case res of
    (Left err, _ ) -> hPutStrLn stderr (pretty err) >> exitWith (ExitFailure 1)
    (_       , ws) -> mapM_ (putStrLn . pretty) ws
