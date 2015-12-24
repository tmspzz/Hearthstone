# heartstone
A Haskell Library and Utilities for Heartstone

Hearstone is a cabal package that contains:

- 1 library: Deckhand
- 2 executables: ``deckhand`` and ``bucaneer``

## Library: Deckhand

Deckhand is composed of 2 modules:

- Deckhand.Deck
- DeckHand.Harpoons.Heartpwn

### Deckhand.Deck

This modules defines the basic types:

- ``type Card = String``
- ``type CardSet = (Card, Int)``
- ``type Deck = [CardSet]`` 

This modules is intended to deal with reading, wiring, diffing and whatever else need to be done on Decks

The ``deckhand`` executable is a concrete example

### Deckhand.Harpoons.Heartpwn
 
This module is intended to deal with grabbing deck information from [Hearthpwn](http://www.hearthpwn.com/)

The ``bucaneer`` executable is a concrete example

## Executable - deckhand

``deckhand`` can diff tow deck files or diff all deck files in a directory.

### Usage
```
$ deckhand diff MyHeartstoneLibrary.csv someDeck.csv
```

This will tell you what you are missing in your card library to make deck "someDeck.csv"

```
$ deckhand mindiff MyHeartstoneLibrary.csv someDir
```

This will give you an ascending list of decks in directory "someDir" according to deck distance (card difference between two decks)

## Executable - bucaneer

``bucaneer`` scrapes decks from [Hearthpwn](http://www.hearthpwn.com/)

### Usage

```
$ buccaneer Hot AllTime
```

This will scrape decks in the Hot and AllTime list
