import Control.Monad.State (evalStateT, lift, unless, when)
import Cryptol.Eval.Value (isTBit)
import Cryptol.FSM
import Cryptol.ModuleM
import Cryptol.Utils.PP (pretty)
import Data.LDAG
import Options
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.FilePath (takeExtension)
import System.IO (hPutStrLn, stderr)

import qualified Convert.LDAG.DOT  as DOT
import qualified Convert.LDAG.JSON as JSON
import qualified Data.Text.Lazy.IO as T

main = do
  opts <- getOpts
  let howToPrint = case optOutputPath opts of
        Just file -> T.writeFile file
        _         -> T.putStrLn
      outputFormat = case optOutputFormat opts of
        Guess -> case takeExtension <$> optOutputPath opts of
          Just ".json" -> JSON
          _            -> DOT
        known -> known

  res <- runModuleM $ do
    when (null (optModules opts)) loadPrelude
    mapM_ loadModuleByPath (optModules opts)
    function <- checkExprSimpleType $ optFunction opts
    let nin  =  inputBits (snd function)
    valid    <- checkExprSimpleType $ validityAscription nin (optValid opts)
    grouping <- optGrouping opts nin
    params   <- getExprBuilderParams
    ldag     <- unfoldLDAGM (checkEquality params (optSolver opts) function valid)
                            (checkDead     params (optSolver opts)          valid)
                            (step nin)
                            []
    io . howToPrint $ case outputFormat of
      DOT  ->  DOT.convert ldag grouping
      JSON -> JSON.convert ldag grouping

  case res of
    (Left err, _ ) -> hPutStrLn stderr (pretty err) >> exitWith (ExitFailure 1)
    (_       , ws) -> mapM_ (putStrLn . pretty) ws
