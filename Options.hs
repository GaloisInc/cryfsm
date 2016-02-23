{-# LANGUAGE ViewPatterns #-}
module Options (OutputFormat(..), Options(..), getOpts) where

import Control.Applicative ((<|>), many)
import Control.Monad (unless)
import Cryptol.Eval.Type (evalType)
import Cryptol.Eval.Value (fromStr, fromSeq, isTSeq, isTBit, numTValue)
import Cryptol.ModuleM (ModuleM, checkExpr, evalExpr, getEvalEnv)
import Cryptol.Parser (parseExpr)
import Cryptol.Symbolic (proverConfigs)
import Cryptol.TypeCheck.AST (Schema(Forall))
import Cryptol.TypeCheck.Solver.InfNat (Nat'(Nat))
import Cryptol.Utils.Ident (packIdent)
import Cryptol.Utils.PP (pretty)
import Data.Aeson (eitherDecode)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.String (fromString)
import Data.Text.Encoding (encodeUtf8)
import qualified Options.Applicative as Opt
import qualified Cryptol.Parser.AST as P

data OutputFormat = DOT | JSON | Guess deriving (Bounded, Enum, Eq, Ord, Read, Show)

data Options = Options
  { optModules      :: [FilePath]
  , optFunction     :: P.Expr P.PName
  , optValid        :: P.Expr P.PName
  , optGrouping     :: ModuleM [String]
  , optOutputPath   :: Maybe FilePath
  , optOutputFormat :: OutputFormat
  , optSolver       :: String
  }

knownSolvers :: [String]
knownSolvers = map fst proverConfigs

knownSolversString = intercalate ", " knownSolvers

knownSolverParser :: Opt.ReadM String
knownSolverParser = do
  s <- Opt.str
  unless (s `elem` knownSolvers) (Opt.readerError $  "unknown solver "
                                                  ++ s
                                                  ++ "; choose from "
                                                  ++ knownSolversString
                                 )
  return s

exprParser :: Opt.ReadM (P.Expr P.PName)
exprParser = do
  s <- Opt.str
  case parseExpr (fromString s) of
    Left err -> Opt.readerError $ "couldn't parse cryptol expression\n" ++ pretty err
    Right v  -> return v

defExpr :: String -> Opt.Parser (P.Expr P.PName)
defExpr s = case parseExpr (fromString s) of
  Left err -> error ("internal error: couldn't parse default expression `" ++ s ++ "`:\n" ++ pretty err)
  Right e  -> pure e

evalStrings :: P.Expr P.PName -> ModuleM [String]
evalStrings parsed = do
  (expr, ty) <- checkExpr parsed
  env        <- getEvalEnv
  assertStrings env ty
  map fromStr . fromSeq <$> evalExpr expr
  where
  assertStrings env schema = case schema of
    Forall [] _ ty -> case evalType env ty of
      (isTSeq -> Just (numTValue -> Nat m, isTSeq -> Just (numTValue -> Nat n, isTSeq -> Just (numTValue -> Nat 8, isTBit -> True))))
        -> return ()
      _ -> fail ("expecting list of strings (some concretization of `{m,n} [m][n][8]`), but type was\n" ++ pretty schema)

stringListParser :: Opt.ReadM (ModuleM [String])
stringListParser = do
  s <- Opt.str
  case eitherDecode . fromString $ s of
    Right json    -> return (return json)
    Left  jsonErr -> case parseExpr . fromString $ s of
      Left  cryptolErr -> Opt.readerError (errors jsonErr cryptolErr)
      Right cryptol    -> return (evalStrings cryptol)
  where
  errors jsonErr cryptolErr = unlines
    [ "tried parsing grouping as JSON, but failed:\n"
    , jsonErr
    , "\ntried parsing grouping as cryptol, but failed:\n"
    , pretty cryptolErr
    ]

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> many (Opt.argument Opt.str (Opt.metavar "FILE ..."))
  <*> (Opt.option exprParser (  Opt.short 'e'
                             <> Opt.metavar "EXPR"
                             <> Opt.help "a cryptol expression to partially evaluate (default `main`)"
                             )
      <|> defExpr "main"
      )
  <*> (Opt.option exprParser (  Opt.short 'v'
                             <> Opt.metavar "EXPR"
                             <> Opt.help "a cryptol expression marking inputs as valid (default `valid`)"
                             )
      <|> defExpr "valid"
      )
  <*> (Opt.option stringListParser (  Opt.short 'g'
                                   <> Opt.metavar "EXPR"
                                   <> Opt.help "a JSON or cryptol expression naming the input positions (default `grouping`)"
                                   )
      <|> (evalStrings <$> defExpr "grouping")
      )
  <*> Opt.optional (Opt.strOption (  Opt.short 'o'
                                  <> Opt.metavar "FILE"
                                  <> Opt.help "output file (default stdout)"
                                  )
                   )
  <*> (Opt.option Opt.auto (  Opt.short 'f'
                           <> Opt.metavar "FORMAT"
                           <> Opt.help (  "output format: "
                                       ++ intercalate ", " (map show [minBound .. maxBound :: OutputFormat])
                                       ++ " (default Guess)"
                                       )
                           )
      <|> pure Guess
      )
  <*> (Opt.option knownSolverParser (  Opt.short 's'
                                    <> Opt.metavar "SOLVER"
                                    <> Opt.help (  "which SMT solver to use: "
                                                ++ knownSolversString
                                                ++ " (default any)"
                                                )
                                    )
      <|> pure "any"
      )

optionsInfo :: Opt.ParserInfo Options
optionsInfo = Opt.info (Opt.helper <*> optionsParser)
  (  Opt.fullDesc
  <> Opt.progDesc "Produce a finite state machine that emulates EXPR, inspecting one input bit at a time, using SOLVER to check equality of states. The type of the main function EXPR to be translated should be monomorphic, and unify with `{a} (Cmp a) => [n] -> a`."
  )

getOpts :: IO Options
getOpts = Opt.execParser optionsInfo
