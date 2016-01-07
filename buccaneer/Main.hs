module Main where

  import qualified System.FilePath.Posix as FP (combine)
  import qualified System.Directory as D
  import qualified Deckhand.Harpoons.Heartpwn as HPWN
  import qualified Deckhand.Deck as DK
  import Data.List (intersperse)
  import Control.Concurrent.Async
  import Data.Functor
  import System.Environment



  type UrlAndFileName = (String, String)

  main :: IO ()
  main = do
    args <- getArgs
    case args of
      [] -> putStrLn $ "You need to specify one or more of " ++ unwords (intersperse "|" (map show [HPWN.Hot, HPWN.New, HPWN.Week, HPWN.Month, HPWN.AllTime]))
      as  -> do
        _ <- mapConcurrently getDecksFromHeartpwnSource (map read as)
        return ()



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
