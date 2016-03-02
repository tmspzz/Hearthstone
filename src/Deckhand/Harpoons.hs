module Deckhand.Harpoons
  ( module H
    , DeckSource(..)
    , fromDeckSource
    , deckSourceFromURI
    , toDeckURL
    , heathpwnDeckURL
    , tempostormDeckURL
    , fileNameFromDeckSource
  )

  where

  import Deckhand.Harpoons.Hearthpwn as H
  import Deckhand.Harpoons.Tempostorm as T
  import qualified Network.URI as N
  import qualified System.FilePath.Posix as FP (takeFileName)
  import qualified Data.List as L (intersperse, intersect, union, intercalate)



  data DeckSource a = Hearthpwn a
    | Tempostorm a

  instance Functor DeckSource where
      fmap f (Hearthpwn a)  = Hearthpwn (f a)
      fmap f (Tempostorm a) = Tempostorm (f a)


  deckSourceFromURI :: N.URI -> Maybe (DeckSource N.URI)
  deckSourceFromURI uri = pure uri
    >>= N.uriAuthority
    >>= pure . N.uriRegName
    >>= \x -> case x of
      "www.hearthpwn.com" -> pure . Hearthpwn $ uri
      "www.tempostorm.com" -> pure . Tempostorm $ uri
      otherwise -> Nothing

  fromDeckSource :: DeckSource a -> a
  fromDeckSource (Hearthpwn a)   = a
  fromDeckSource (Tempostorm a ) = a

  toDeckURL :: DeckSource String -> String -> String
  toDeckURL source s = fromDeckSource $ fmap (++s) source

  heathpwnDeckURL :: DeckSource String
  heathpwnDeckURL = Hearthpwn H.hearthpwnBaseURL

  tempostormDeckURL :: DeckSource String
  tempostormDeckURL = Tempostorm T.tempostormBaseURL

  fileNameFromDeckSource :: DeckSource N.URI -> String
  fileNameFromDeckSource (Hearthpwn uri ) = L.intercalate "-" . tail . H.wordsWhen (=='-') . FP.takeFileName $ N.uriPath uri
  fileNameFromDeckSource (Tempostorm uri) = FP.takeFileName $ N.uriPath uri
