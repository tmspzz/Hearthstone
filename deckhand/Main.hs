module Main where

  import System.Environment
  import System.Directory (getDirectoryContents)
  import System.FilePath as FP (combine)
  import Data.Functor
  import Data.List (sortBy)
  import Data.Ord (comparing, compare)
  import qualified Deckhand.Deck as DK
  import Data.Function (on)

  main :: IO ()
  main = getArgs >>= parseArgs


  diffUsage :: String
  diffUsage = "Usage: deckhand diff your_cards.csv a_deck_file"

  mindiffUsage :: String
  mindiffUsage = "Usage: deckhand mindiff your_card.sv dir_with_deck_files/"

  parseArgs :: [String] -> IO ()
  parseArgs [] = do
                putStrLn diffUsage
                putStrLn mindiffUsage
  parseArgs args | length args <= 2 = case head args of
                                        "diff" -> putStrLn diffUsage
                                        "mindiff" -> putStrLn mindiffUsage
                                        otherwise -> putStrLn $ unwords $ "Unrecognized command: ":args
  parseArgs [cmd, ownDeck, path] | cmd == "diff" = do
                                      diffDeck <-  path `diffDeckFiles` ownDeck
                                      let numMissingCards = length diffDeck
                                      putStrLn $ "You are missing " ++ show numMissingCards ++ " cards:"
                                      putStrLn ""
                                      putStrLn $ unlines $ map show diffDeck
                                      putStrLn ""
                                 | cmd == "mindiff" = do
                                      contentsOfDir <- getDirectoryContents path
                                      let allFiles = [path `FP.combine` f | f<- contentsOfDir, f /= "." && f /= ".."]
                                      intDiffs <- mapM (diffDeckFiles' ownDeck) allFiles
                                      let filesAndScore = zip allFiles intDiffs
                                      putStrLn $ unlines $ map show (sortBy (compare `on` snd) filesAndScore)
                                 | otherwise = putStrLn $ "Unrecognized command: " ++ cmd


  diffDeckFiles :: FilePath -> FilePath -> IO DK.Deck
  diffDeckFiles path1 path2  = do
                            deck1 <- DK.fromFile path1
                            deck2 <- DK.fromFile path2
                            return $ deck1 `DK.difference` deck2

  -- This is inverted WRT the noral parameter oder of DK.difference' beucase
  -- of the mapM above
  diffDeckFiles' :: FilePath -> FilePath -> IO Int
  diffDeckFiles' path1 path2 = do
                            deck1 <- DK.fromFile path1
                            deck2 <- DK.fromFile path2
                            return $ DK.difference' deck2 deck1
