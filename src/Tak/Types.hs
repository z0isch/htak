module Tak.Types where

type Rank = Int
type File = Char
type Coord = (File, Rank)

data Player = Player1 | Player2
  deriving (Eq, Show)

data Direction = L | R | U | D
  deriving (Eq, Show)

data PieceType = Flat | Standing | Cap
  deriving (Eq, Show)

data Move = Place PieceType Coord
          | Move Int Coord Direction [Int]
          | RoadWin Player
          | FlatWin Player
          | ResignWin Player
          | Draw
  deriving (Eq, Show)
