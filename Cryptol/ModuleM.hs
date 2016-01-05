module Cryptol.ModuleM
  ( ModuleM
  , liftCmd
  , io
  , runModuleM
  , checkExpr
  , evalExpr
  , satProve
  , loadModuleByPath
  , loadPrelude
  , getEvalEnv
  , getPrimMap
  , renameInteractive
  , typeCheckInteractive
  ) where

import Cryptol.ModuleSystem (ModuleCmd, initialModuleEnv)
import Cryptol.ModuleSystem.Base (TCAction(TCAction, tcAction, tcLinter, tcPrims), evalExpr, exprLinter, getPrimMap, rename, typecheck)
import Cryptol.ModuleSystem.Monad (ImportSource(FromModule), ModuleM, ModuleT(ModuleT), getEvalEnv, io)
import Cryptol.ModuleSystem.Renamer (RenameM)
import Cryptol.Symbolic (ProverCommand, ProverResult)
import Cryptol.TypeCheck (tcExpr)
import Cryptol.Utils.Ident (preludeName, interactiveName)
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

renameInteractive :: RenameM a -> ModuleM a
renameInteractive act = do
  (_, namingEnv, _) <- Base.getFocusedEnv
  rename interactiveName namingEnv act

typeCheckInteractive :: P.Expr TC.Name -> ModuleM (TC.Expr, TC.Schema)
typeCheckInteractive expr = do
  pm <- getPrimMap
  (ifaceDecls, _, _) <- Base.getFocusedEnv
  let act = TCAction { tcAction = tcExpr, tcLinter = exprLinter, tcPrims = pm }
  Base.loading (FromModule interactiveName) (typecheck act expr ifaceDecls)
