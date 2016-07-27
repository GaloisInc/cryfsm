import Data.Aeson (eitherDecode, encode)
import Data.List (findIndex, intercalate)
import Data.MBP (HasDimensions(dims), MBP, branches, elems, matrix, mbp, outputs, position, step, steps)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Data.Monoid ((<>))
import System.Exit (die)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text            as T
import qualified Options.Applicative  as Opt

data Options = Options
  { source    :: Maybe FilePath
  , target    :: Maybe FilePath
  , selection :: Text
  }
  deriving (Eq, Ord, Read, Show)

optionsParser :: Opt.Parser Options
optionsParser = Options
  <$> Opt.optional (Opt.strOption (  Opt.short 'i'
                                  <> Opt.metavar "FILE"
                                  <> Opt.help "input file (default stdin)"
                                  )
                   )
  <*> Opt.optional (Opt.strOption (  Opt.short 'o'
                                  <> Opt.metavar "FILE"
                                  <> Opt.help "output file (default stdout)"
                                  )
                   )
  <*> (T.pack <$> Opt.strArgument (  Opt.metavar "STRING"
                                  <> Opt.help "matrix branching program output to specialize for"
                                  )
      )

optionsInfo :: Opt.ParserInfo Options
optionsInfo = Opt.info (Opt.helper <*> optionsParser)
  (  Opt.fullDesc
  <> Opt.progDesc "Turn a multi-output matrix branching program into a single-output one"
  )

loadSource :: Maybe FilePath -> IO MBP
loadSource source = do
  bs <- maybe BS.getContents BS.readFile source
  either die return (eitherDecode bs)

specialize :: Text -> MBP -> Either String MBP
specialize selection m = do
  case dims (outputs m) of
    (1, _) -> return ()
    _      -> multiRowOutput
  i <- maybe unknownOutput return $ findIndex (selection==) (head (elems (outputs m)))
  maybe (columnSelectionFailed i) return $ filterMBP i m
  where
  filterRow    i   = take 1 . drop i
  filterElems  i   = map (filterRow i)
  filterMatrix i   = matrix . filterElems i . elems
  filterMap    i   = traverse (filterMatrix i)
  filterStep   i s = filterMap i (branches s) >>= step (position s)
  filterMBP    i m = do
    last:init <- return (reverse (steps m))
    last'     <- filterStep   i last
    output'   <- filterMatrix i (outputs m)
    mbp (reverse (last':init)) output'

  multiRowOutput = Left "Turning multi-row-output programs into evasive programs is not yet implemented."
  unknownOutput  = Left
    $  "The specified matrix branching program does not have "
    <> T.unpack selection
    <> "\nas an output. Known outputs are:\n"
    <> (intercalate "\n" . concat . map (map T.unpack) . elems) (outputs m)
  columnSelectionFailed i = Left
    $  "The impossible happened: selecting column "
    <> show i
    <> " from the last matrix of the program violated some invariant."

saveTarget :: Maybe FilePath -> MBP -> IO ()
saveTarget target mbp = maybe BS.putStr BS.writeFile target (encode mbp)

main :: IO ()
main = do
  opts <- Opt.execParser optionsInfo
  mbp  <- loadSource (source opts)
  case specialize (selection opts) mbp of
    Left  err  -> die err
    Right mbp' -> saveTarget (target opts) mbp'
