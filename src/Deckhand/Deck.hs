module Deckhand.Deck
  ( fromTuple
    , fromFile
    , toFile
    , difference
    , difference'
    , Card
    , CardSet
    , Deck
  )
where

  import System.IO (FilePath)
  import qualified Text.CSV as T
  import qualified Data.Csv as D
  import qualified Data.Map as M
  import qualified Data.ByteString as B
  import qualified Data.ByteString.Lazy as BL
  import qualified Data.Vector as V
  import Data.Functor
  import Control.Applicative


  type Card    = String
  type CardSet = (String, Int)
  type Deck    = [CardSet]



  fromTuple :: (String, String) -> CardSet
  fromTuple t = (fst t, read $ snd t)

  fromFile :: FilePath -> IO (Deck)
  fromFile path = do
    eitherErorrOrCVS <- T.parseCSVFromFile path
    case eitherErorrOrCVS of
      Left error -> return []
      Right csv -> return $ parseDeck csv

  parseDeck :: T.CSV -> Deck
  parseDeck [] = []
  parseDeck (r:rs) = case parseCardSet r of
                       Nothing  -> parseDeck rs
                       Just set -> set : parseDeck rs

  parseCardSet :: T.Record -> Maybe CardSet
  parseCardSet r
                | length r >= 2 = Just ( r !! 0, read $ r !! 1)
                | otherwise    = Nothing
{-
  parseDeck :: V.Vector (String,String,String) -> Deck
  parseDeck v = V.toList $ parseOneCardSet <$> v

  parseOneCardSet :: (String, String, String) -> CardSet
  parseOneCardSet (n, q, _) = (n, read q)

  parseDeck'   :: V.Vector (String,String) -> Deck
  parseDeck' v = V.toList $ parseOneCardSet' <$> v

  parseOneCardSet'       :: (String, String) -> CardSet
  parseOneCardSet' (n,q) = (n, read q)
-}
  difference :: Deck -> Deck -> Deck
  difference d1 d2 = let d1Map = M.fromList d1
                         d2Map = M.fromList d2
                         differenceMap = M.difference d1Map d2Map
                     in M.toList differenceMap

  difference' :: Deck -> Deck -> Int
  difference' d1 d2 = length $ d1 `difference` d2

  toFile :: Deck -> FilePath -> IO ()
  toFile deck path = do
    BL.writeFile path $ D.encode deck
    return ()

