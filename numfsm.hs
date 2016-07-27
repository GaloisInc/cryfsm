import Control.Monad.State (StateT, get, put, runStateT)
import Control.Monad.Writer (Writer, runWriter, tell)
import Data.Aeson (FromJSON(parseJSON), Value(Number, Object), (.:), eitherDecode, encode)
import Data.Aeson.Types (typeMismatch)
import Data.Foldable (fold, traverse_)
import Data.List (nub)
import Data.Maybe (fromMaybe)
import Data.MBP (MBP, Position, Step, Symbol, branches, position, steps)
import Data.Monoid ((<>))
import Data.Set (Set)
import Data.String (fromString)
import Data.Text (Text, unpack)
import Data.TotalMap (TMap, fromPartial)
import System.Exit (die)
import System.IO (hPutStrLn, stderr)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Map             as M
import qualified Data.HashMap.Strict  as H
import qualified Data.Set             as S
import qualified Data.TotalMap        as TM
import qualified Options.Applicative  as Opt

newtype Source = Source (TMap Position (Maybe Integer))

data Options target = Options
  { source :: Source
  , target :: target
  }

instance FromJSON Source where
  parseJSON n@(Number _) = Source . pure . pure <$> parseJSON n
  parseJSON o@(Object _) = Source . fromPartial Nothing . fmap Just <$> parseJSON o
  parseJSON v = typeMismatch "number or map from positions to numbers" v

loadTarget :: Options FilePath -> IO (Options MBP)
loadTarget o = do
  bs <- LBS.readFile (target o)
  case eitherDecode bs of
    Right mbp -> pure (o { target = mbp })
    Left  err -> die $ "Could not read state machine in " ++ target o ++ ":\n" ++ err

sourceParser :: Opt.ReadM Source
sourceParser = do
  s <- Opt.str
  case eitherDecode . fromString $ s of
    Right source -> return source
    Left  err    -> fail $ "tried parsing source as JSON, but failed:\n" ++ err

optionsParser :: Opt.Parser (Options FilePath)
optionsParser = Options
  <$> Opt.argument sourceParser
        (  Opt.metavar "SOURCE"
        <> Opt.help "JSON value with a number to rebase"
        )
  <*> Opt.argument Opt.str
        (  Opt.metavar "TARGET"
        <> Opt.help "JSON file with a description of a state machine to rebase for"
        )

optionsInfo :: Opt.ParserInfo (Options FilePath)
optionsInfo = Opt.info (Opt.helper <*> optionsParser)
  (  Opt.fullDesc
  <> Opt.progDesc "Convert base-10 numbers into suitable symbols for use with a state machine."
  )

data Warning = Unused | Underused deriving (Bounded, Enum, Eq, Ord, Read, Show)

showWarning :: Warning -> String
showWarning Unused    = "Some positions found in the source were not mentioned in the target."
showWarning Underused = "Some positions found in the source were not completely consumed while rebasing."

set :: Ord k => k -> v -> TMap k v -> TMap k v
set k v tm = fromMaybe <$> tm <*> fromPartial Nothing (M.singleton k (Just v))

step :: Step -> StateT Source (Writer [Position]) Symbol
step s = do
  Source tm <- get
  case tm TM.! position s of
    Nothing -> tell [position s] >> return mempty
    Just n  -> case divMod n (fromIntegral (M.size (branches s))) of
      (n', i) -> put (Source (set (position s) (Just n') tm))
              >> return (fst (M.elemAt (fromIntegral i) (branches s)))

rebase :: Options MBP -> Either [Position] (Set Warning, [Symbol])
rebase Options { source = s@(Source s_), target = mbp }
  = case runWriter (runStateT (mapM step (reverse (steps mbp))) s) of
      (_                   , ps@(_:_)) -> Left  (nub ps)
      ((result, Source s'_), []      ) -> Right (warning, reverse result) where
        warning
          =    fold (TM.codomain (findWarning <$> s_ <*> s'_))
          S.\\ if S.member Nothing (TM.codomain s_)
                 then S.empty
                 else S.singleton Unused

        findWarning Nothing  _         = S.empty
        findWarning (Just v) (Just 0 ) = S.empty
        findWarning (Just v) (Just v') = S.singleton
          (if v == v' then Unused else Underused)

main :: IO ()
main = do
  opts <- Opt.execParser optionsInfo >>= loadTarget
  case rebase opts of
    Left  missing -> die
      $  "The following positions were found in the target but not in the source:\n"
      ++ unlines (map unpack missing)
    Right (warning, result) -> do
      traverse_ (hPutStrLn stderr . showWarning) warning
      LBS.putStr (encode result)
