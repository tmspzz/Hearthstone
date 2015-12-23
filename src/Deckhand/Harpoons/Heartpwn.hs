module Deckhand.Harpoons.Heartpwn
  ( HeartpwnDeckSource(..)
    , urlFromHeartpwnDeckSource
    , toDeckURL
    , getDeck
    , getDeckURIsFromHeartpwnDeckSource
    , fileNameFromDeckURI
  )
where

  import Network.HTTP.Conduit
  import Text.XML.HXT.Core
  import Text.HandsomeSoup
  import qualified Data.List as L (intersperse, intersect, union)
  import qualified Data.ByteString.Lazy.Char8 as B
  import Data.Char
  import Data.Functor
  import Deckhand.Deck as DK
  import qualified System.FilePath.Posix as FP (takeFileName)

  data HeartpwnDeckSource = Hot | New | Week | Month | AllTime deriving (Show, Read, Eq)

  idFromHeartpwnDeckSource :: HeartpwnDeckSource -> String
  idFromHeartpwnDeckSource Hot = show 1
  idFromHeartpwnDeckSource New = show 2
  idFromHeartpwnDeckSource Week = show 3
  idFromHeartpwnDeckSource Month = show 4
  idFromHeartpwnDeckSource AllTime = show 5

  heartpwnBaseURL = "http://www.hearthpwn.com"
  heartpwnDecksSearchURL = heartpwnBaseURL ++ "/decks?filter-deck-tag="

  urlFromHeartpwnDeckSource :: HeartpwnDeckSource -> String
  urlFromHeartpwnDeckSource source = heartpwnDecksSearchURL ++ idFromHeartpwnDeckSource source ++ "&cookieTest=1"

  toDeckURL   :: String -> String
  toDeckURL a = heartpwnBaseURL ++ a

  getDeckURIsFromHeartpwnDeckSource source = do
    let doc = fromUrl $  urlFromHeartpwnDeckSource source
    partialDeckURLs <- runX $ doc >>> css "span.tip" >>> css "a" ! "href"
    return partialDeckURLs

  fileNameFromDeckURI = (concat . L.intersperse "-" . tail . splitAtDash . FP.takeFileName)

  getDeck url = do
    page <- simpleHttp url
    deck <- parseDeckFromHTML $ readString [withParseHTML yes, withWarnings no] $ B.unpack page
    return deck

  parseDeckFromHTML doc = do
    listOfNamesAndQuantities <- runX $ doc
                                        >>> css "table.listing-cards-tabular"
                                        //> hasAttr "data-count"
                                        >>> ( (multi getText >>. map removeUnwatedChars) &&& getAttrValue "data-count" )
    return $ DK.fromTuple <$> listOfNamesAndQuantities


  removeUnwatedChars :: String -> String
  removeUnwatedChars = reverse . dropWhile (isSpace) . reverse . dropWhile (isSpace)

  wordsWhen     :: (Char -> Bool) -> String -> [String]
  wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

  splitAtDash = wordsWhen (=='-')

