module Main where

  import qualified System.FilePath.Posix as FP (combine)
  import qualified System.Directory as D
  import qualified Deckhand.Harpoons as HRP
  import qualified Deckhand.Deck as DK
  import qualified Network.URI as N
  import Data.List (intersperse, nub)
  import Data.Maybe
  import Data.Functor
  import System.Environment
  import Control.Concurrent.Async
  import Control.Monad
  import Control.Monad.Trans
  import Control.Monad.Trans.Maybe
  import Options.Applicative


-- Data Types
  type UrlAndFileName = (String, String)

  data Command
    = Rip [String]
    | Scrape [String]



-- Command line parsing
  parseCommand :: Parser Command
  parseCommand = subparser $
          command "rip"    (parseRip `withInfo` "Rip one of more deck URLs") <>
          command "scrape" (parseScrape `withInfo` ("Scrape a Hearthpwn source for all decks. One or more of " ++ sources))
          where
            sources = unwords (intersperse "|" (map show [HRP.Hot, HRP.New, HRP.Week, HRP.Month, HRP.AllTime]))

  withInfo :: Parser a -> String -> ParserInfo a
  withInfo opts desc = info (helper <*> opts) $ progDesc desc

  parseRip :: Parser Command
  parseRip = Rip <$> some (argument str (metavar "[URL ...]"))

  parseScrape :: Parser Command
  parseScrape = Scrape <$> some (argument str (metavar "[SOURCE ...]"))



-- Main
  main :: IO ()
  main = do
    let opts = info (helper <*> parseCommand) (header "Rips decks for cards" )
    cmd <- execParser opts
    case cmd of
      Rip urls -> void $ mapConcurrently (runMaybeT . getDeckFromURL) (nub urls)
      Scrape sources -> void $ mapConcurrently getDecksFromHearthpwnSource (map (read . nub) sources)

-- Core
  getDeckFromURL :: String -> MaybeT IO ()
  getDeckFromURL urlString = MaybeT (parseURL urlString)
    >>= MaybeT . return . HRP.deckSourceFromURI
    >>= lift . getDeckFromDeckSource

  parseURL :: String -> IO (Maybe N.URI)
  parseURL urlString = do
    uri <- return . N.parseURI $ urlString
    when (isNothing uri) (putStrLn $ "Bad URL: " ++ urlString)
    return uri

  deckSourceFromURI :: N.URI -> IO (Maybe (HRP.DeckSource N.URI))
  deckSourceFromURI uri = do
    source <- return . HRP.deckSourceFromURI $ uri
    when (isNothing source) (putStrLn $ "Unsupported URL: " ++ url)
    return source
    where
      url = N.uriToString id uri ""

  getDeckFromDeckSource :: HRP.DeckSource N.URI -> IO ()
  getDeckFromDeckSource deckSourceURI  = do
    filesInCache <- getFilesFromCache
    when (fileName `notElem` filesInCache) $ saveDeckToFile (HRP.Hearthpwn urlAndFileName)
    where
      fileName = HRP.fileNameFromDeckSource deckSourceURI
      url = N.uriToString id (HRP.fromDeckSource deckSourceURI) ""
      urlAndFileName = (url, fileName)

  getDecksFromHearthpwnSource :: HRP.HearthpwnDeckSource -> IO ()
  getDecksFromHearthpwnSource source = do
      partialDeckURLs <- HRP.getDeckURIsFromHearthpwnDeckSource source
      let completeDecksURLs = HRP.toDeckURL HRP.heathpwnDeckURL <$> partialDeckURLs
      let fileNames = HRP.fileNameFromDeckURI <$> partialDeckURLs
      filesInCache <- getFilesFromCache
      let urlsAndFiles = zip completeDecksURLs fileNames -- [( http:// ... , some file name extracted from the url)]
      let urlsAndFilesToFetch = [ (url, fileName) | (url, fileName) <- urlsAndFiles, fileName `notElem` filesInCache]
      mapM_ saveDeckToFile $ HRP.Hearthpwn <$> urlsAndFilesToFetch

  saveDeckToFile ::  HRP.DeckSource UrlAndFileName -> IO ()
  saveDeckToFile (HRP.Hearthpwn (url, fileName)) = putStrLn ("*** Getting deck at " ++ url)
    >> HRP.getDeck url
    >>= \ d -> DK.toFile d (deckhandDeckCache `FP.combine` fileName)
  saveDeckToFile (HRP.Tempostorm (url, fileName)) = undefined

  getFilesFromCache :: IO [FilePath]
  getFilesFromCache = do
        D.createDirectoryIfMissing True deckhandDeckCache
        files <- D.getDirectoryContents deckhandDeckCache
        return [f | f <- files, f /= "." && f /= ".."]

  deckhandCache :: FilePath
  deckhandCache = ".deckhand"

  deckhandDeckCache :: FilePath
  deckhandDeckCache = deckhandCache `FP.combine` "bucaneer" `FP.combine` "decks"
