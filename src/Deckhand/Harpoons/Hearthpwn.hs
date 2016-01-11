module Deckhand.Harpoons.Hearthpwn
  ( HearthpwnDeckSource(..)
    , urlFromHearthpwnDeckSource
    , toDeckURL
    , getDeck
    , getDeckURIsFromHearthpwnDeckSource
    , fileNameFromDeckURI
  )
where

  import Network.HTTP.Conduit
  import Text.XML.HXT.Core
  import Text.HandsomeSoup
  import qualified Data.List as L (intersperse, intersect, union)
  import qualified Data.ByteString.Lazy.Char8 as B
  import Data.Char
  import Data.List (intercalate)
  import Data.Functor
  import Deckhand.Deck as DK
  import qualified System.FilePath.Posix as FP (takeFileName)

  data HearthpwnDeckSource = Hot | New | Week | Month | AllTime deriving (Show, Read, Eq)

  idFromHearthpwnDeckSource :: HearthpwnDeckSource -> String
  idFromHearthpwnDeckSource Hot = show 1
  idFromHearthpwnDeckSource New = show 2
  idFromHearthpwnDeckSource Week = show 3
  idFromHearthpwnDeckSource Month = show 4
  idFromHearthpwnDeckSource AllTime = show 5

  hearthpwnBaseURL :: String
  hearthpwnBaseURL = "http://www.hearthpwn.com"

  hearthpwnDecksSearchURL :: String
  hearthpwnDecksSearchURL = hearthpwnBaseURL ++ "/decks?filter-deck-tag="

  urlFromHearthpwnDeckSource :: HearthpwnDeckSource -> String
  urlFromHearthpwnDeckSource source = hearthpwnDecksSearchURL ++ idFromHearthpwnDeckSource source ++ "&cookieTest=1"

  toDeckURL   :: String -> String
  toDeckURL a = hearthpwnBaseURL ++ a

  getDeckURIsFromHearthpwnDeckSource source = runX $ doc
    >>> css "span.tip"
    >>> css "a" ! "href"
      where
        doc = fromUrl $  urlFromHearthpwnDeckSource source

  fileNameFromDeckURI = intercalate "-" . tail . splitAtDash . FP.takeFileName

  getDeck url = do
    page <- simpleHttp url
    parseDeckFromHTML $ readString [withParseHTML yes, withWarnings no] $ B.unpack page

  parseDeckFromHTML doc = do
    listOfNamesAndQuantities <- runX $ doc
                                        >>> css "table.listing-cards-tabular"
                                        //> hasAttr "data-count"
                                        >>> ( (multi getText >>. map removeUnwatedChars) &&& getAttrValue "data-count" )
    return $ DK.fromTuple <$> listOfNamesAndQuantities


  removeUnwatedChars :: String -> String
  removeUnwatedChars = reverse . dropWhile isSpace . reverse . dropWhile isSpace

  wordsWhen     :: (Char -> Bool) -> String -> [String]
  wordsWhen p s = case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

  splitAtDash = wordsWhen (=='-')
