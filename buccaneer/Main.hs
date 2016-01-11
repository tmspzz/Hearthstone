module Main where

  import qualified System.FilePath.Posix as FP (combine)
  import qualified System.Directory as D
  import qualified Deckhand.Harpoons.Heartpwn as HPWN
  import qualified Deckhand.Deck as DK
  import qualified Network.URI as N
  import Data.List (intersperse, nub)
  import Control.Concurrent.Async
  import Data.Functor
  import System.Environment
  import Options.Applicative
  import Control.Monad
  import Control.Monad.Trans
  import Control.Monad.Trans.Maybe


  parseCommand :: Parser Command
  parseCommand = subparser $
          command "rip"    (parseRip `withInfo` "Rip one of more deck URLs") <>
          command "scrape" (parseScrape `withInfo` ("Scrape a Heartpwn source for all decks. One or more of " ++ sources))
          where
            sources = unwords (intersperse "|" (map show [HPWN.Hot, HPWN.New, HPWN.Week, HPWN.Month, HPWN.AllTime]))

  withInfo :: Parser a -> String -> ParserInfo a
  withInfo opts desc = info (helper <*> opts) $ progDesc desc

  parseRip :: Parser Command
  parseRip = Rip <$> some (argument str (metavar "[URL ...]"))

  parseScrape :: Parser Command
  parseScrape = Scrape <$> some (argument str (metavar "[SOURCE ...]"))

  type UrlAndFileName = (String, String)

  data Command
    = Rip [String]
    | Scrape [String]

  main :: IO ()
  main = do
    let opts = info (helper <*> parseCommand) (header "Rips decks for cards" )
    cmd <- execParser opts
    case cmd of
      Rip urls -> void $ mapConcurrently (runMaybeT . getDeckFromURL) (nub urls)
      Scrape sources -> void $ mapConcurrently getDecksFromHeartpwnSource (map (read . nub) sources)


  getDeckFromURL :: String -> MaybeT IO ()
  getDeckFromURL urlString = MaybeT (pure . N.parseURI$ urlString)
    >>= getDeckFromURI

  getDeckFromURI :: N.URI -> MaybeT IO ()
  getDeckFromURI uri = MaybeT ( pure . N.uriAuthority $ uri)
    >>= pure . N.uriRegName
    >>= \x ->  case x of
      "www.hearthpwn.com" -> lift . getDeckFromHeartpwnURI $ uri
      "www.tempostorm.com" -> lift . getDeckFromTempostormURI $ uri
      otherwise -> lift $ void (putStrLn ("Unsupported URL " ++ N.uriToString id uri ""))

  getDeckFromHeartpwnURI :: N.URI -> IO ()
  getDeckFromHeartpwnURI uri = do
    filesInCache <- getFilesFromCache
    saveDeckToFile urlAndFileName
    where
      fileName = HPWN.fileNameFromDeckURI $ N.uriPath uri
      url = N.uriToString id uri ""
      urlAndFileName = (url, fileName)

  getDeckFromTempostormURI :: N.URI -> IO ()
  getDeckFromTempostormURI uri = undefined

  getDecksFromHeartpwnSource :: HPWN.HeartpwnDeckSource -> IO ()
  getDecksFromHeartpwnSource source = do
      partialDeckURLs <- HPWN.getDeckURIsFromHeartpwnDeckSource source
      let completeDecksURLs = HPWN.toDeckURL <$> partialDeckURLs
      let fileNames = HPWN.fileNameFromDeckURI <$> partialDeckURLs
      filesInCache <- getFilesFromCache
      let urlsAndFiles = zip completeDecksURLs fileNames -- [( http:// ... , some file name extracted from the url)]
      let urlsAndFilesToFetch = [ (url, fileName) | (url, fileName) <- urlsAndFiles, fileName `notElem` filesInCache]
      mapM_ saveDeckToFile urlsAndFilesToFetch

  saveDeckToFile :: UrlAndFileName -> IO ()
  saveDeckToFile (url, fileName) = putStrLn ("*** Getting deck at " ++ url) >> HPWN.getDeck url >>= (\d -> DK.toFile d (deckhandDeckCache `FP.combine` fileName))

  getFilesFromCache :: IO [FilePath]
  getFilesFromCache = do
        D.createDirectoryIfMissing True deckhandDeckCache
        files <- D.getDirectoryContents deckhandDeckCache
        return [f | f<- files, f /= "." && f /= ".."]

  deckhandCache :: FilePath
  deckhandCache = ".deckhand"

  deckhandDeckCache :: FilePath
  deckhandDeckCache = deckhandCache `FP.combine` "bucaneer" `FP.combine` "decks"
