import Control.Monad.State (evalStateT, lift, unless, when)
import Cryptol.Eval.Value (isTBit)
import Cryptol.FSM
import Cryptol.ModuleM
import Cryptol.Utils.PP (pretty)
import Data.LDAG
import Options
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

import qualified Convert.LDAG.DOT  as DOT
import qualified Convert.LDAG.JSON as JSON
import qualified Data.Text.Lazy.IO as T

main = do
  opts <- getOpts
  let howToPrint = case optOutputPath opts of
        Just file -> T.writeFile file
        _         -> T.putStrLn
  res <- runModuleM $ do
    when (null (optModules opts)) loadPrelude
    mapM_ loadModuleByPath (optModules opts)
    function <- checkExprSimpleType $ optFunction opts
    let nin  =  inputBits (snd function)
    valid    <- checkExprSimpleType $ validityAscription nin (optValid opts)
    grouping <- optGrouping opts
    params   <- getExprBuilderParams
    ldag     <- unfoldLDAGM (checkEquality params (optSolver opts) function valid)
                            (checkDead     params (optSolver opts)          valid)
                            (step nin)
                            []
    io . howToPrint $ case optOutputFormat opts of
      -- TODO: use the grouping information to make clusters in DOT.convert
      DOT  ->  DOT.convert ldag
      JSON -> JSON.convert ldag grouping
  case res of
    (Left err, _ ) -> hPutStrLn stderr (pretty err) >> exitWith (ExitFailure 1)
    (_       , ws) -> mapM_ (putStrLn . pretty) ws
