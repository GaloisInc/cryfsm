module Options (OutputFormat(..), Options(..), getOpts) where

import Control.Applicative ((<|>), many)
import Control.Monad (unless)
import Cryptol.Parser (parseExpr)
import Cryptol.Symbolic (proverConfigs)
import Cryptol.Utils.Ident (packIdent)
import Cryptol.Utils.PP (pretty)
import Data.List (intercalate)
import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Options.Applicative as Opt
import qualified Cryptol.Parser.AST as P

data OutputFormat = DOT deriving (Bounded, Enum, Eq, Ord, Read, Show)

-- TODO: put in a more sane order, maybe
data Options = Options
  { optOutputPath   :: Maybe FilePath
  , optFunction     :: P.Expr P.PName
  , optValid        :: Maybe (P.Expr P.PName)
  -- TODO: , optGrouping     :: [String]
  , optOutputFormat :: OutputFormat
  , optSolver       :: String
  , optModules      :: [FilePath]
  } deriving (Eq, Show)

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

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> Opt.optional (Opt.strOption (  Opt.short 'o'
                                  <> Opt.metavar "FILE"
                                  <> Opt.help "output file (default stdin)"
                                  )
                   )
  <*> (Opt.option exprParser (  Opt.short 'e'
                             <> Opt.metavar "EXPR"
                             <> Opt.help "a cryptol expression to partially evaluate (default `main`)"
                             )
      <|> pure (P.EVar . P.UnQual . packIdent $ "main")
      )
  <*> Opt.optional (Opt.option exprParser (  Opt.short 'v'
                                          <> Opt.metavar "EXPR"
                                          <> Opt.help "a cryptol expression marking inputs as valid (default `\\_ -> True`)"
                                          )
                   )
  <*> (Opt.option Opt.auto (  Opt.short 'f'
                           <> Opt.metavar "FORMAT"
                           <> Opt.help (  "output format: "
                                       ++ intercalate ", " (map show [minBound .. maxBound :: OutputFormat])
                                       ++ " (default DOT)"
                                       )
                           )
      <|> pure DOT
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
  <*> many (Opt.argument Opt.str (Opt.metavar "FILE ..."))

optionsInfo :: Opt.ParserInfo Options
optionsInfo = Opt.info (Opt.helper <*> optionsParser)
  (  Opt.fullDesc
  <> Opt.progDesc "Produce a finite state machine that emulates EXPR, inspecting one input bit at a time, using SOLVER to check equality of states. The type of EXPR should be monomorphic, and unify with `{a} (Cmp a) => [n] -> a`."
  )

getOpts :: IO Options
getOpts = Opt.execParser optionsInfo
