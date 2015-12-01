module Cryptol.ModuleM
  ( ModuleM
  , liftCmd
  , io
  , runModuleM
  , checkExpr
  , satProve
  , loadModuleByPath
  , loadPrelude
  , getEvalEnv
  ) where

import Cryptol.ModuleSystem (ModuleCmd, initialModuleEnv)
import Cryptol.ModuleSystem.Monad (ModuleM, ModuleT(ModuleT), getEvalEnv, io)
import Cryptol.Symbolic (ProverCommand, ProverResult)
import Cryptol.Utils.Ident (preludeName)
import MonadLib (get, inBase, put, raise, set)
import qualified Cryptol.ModuleSystem       as Cmd
import qualified Cryptol.ModuleSystem.Monad as Base
import qualified Cryptol.Parser.AST         as P
import qualified Cryptol.Symbolic           as Symbolic
import qualified Cryptol.TypeCheck.AST      as TC

liftCmd :: ModuleCmd a -> ModuleM a
liftCmd f = ModuleT $ do
  env <- get
  (res, ws) <- inBase (f env)
  put ws
  case res of
    Left err -> raise err
    Right (val, env') -> val <$ set env'

checkExpr :: P.Expr P.PName -> ModuleM (TC.Expr, TC.Schema)
checkExpr parsed = (\(_, e, s) -> (e, s)) <$> liftCmd (Cmd.checkExpr parsed)

satProve :: ProverCommand -> ModuleM ProverResult
satProve = liftCmd . Symbolic.satProve

findModule :: P.ModName -> ModuleM FilePath
findModule = liftCmd . Cmd.findModule

loadModuleByPath :: FilePath -> ModuleM TC.Module
loadModuleByPath = liftCmd . Cmd.loadModuleByPath

runModuleM :: ModuleM a -> IO (Cmd.ModuleRes a)
runModuleM act = do
  env <- initialModuleEnv
  Base.runModuleM env act

loadPrelude :: ModuleM ()
loadPrelude = findModule preludeName >>= loadModuleByPath >> return ()
