{-# LANGUAGE TemplateHaskell #-}

module Tak.Types where

import           Control.Lens
import           Data.DList      (DList)
import qualified Data.DList      as DL
import           Data.List
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import           Safe            (headMay)

type Rank = Int
type File = Char
type Coord = (File, Rank)
type BoardSize = Int

data Player = Player1 | Player2
  deriving (Eq, Show, Ord)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

data Direction = L | R | U | D
  deriving (Eq, Show)

goDirection :: Direction -> Coord -> Coord
goDirection U = _2 %~ succ
goDirection D = _2 %~ pred
goDirection R = _1 %~ succ
goDirection L = _1 %~ pred

data PieceType = Flat | Standing | Cap
  deriving (Eq, Show)

data Move = Place PieceType Coord
          | Move Int Coord Direction [Int]
          | RoadWin Player
          | FlatWin Player
          | ResignWin Player
          | Draw
  deriving (Eq, Show)

data GameState = GameState
  { _gsBoard        :: Map Coord [(Player,PieceType)]
  , _gsMoves        :: DList Move
  , _gsCurrPlayer   :: Player
  , _gsSupplyPieces :: Map Player (Int, Int)
  }
  deriving (Eq,Show)
makeLenses ''GameState

initialGameState :: BoardSize -> GameState
initialGameState s = GameState
  { _gsBoard = foldl' (\m c -> M.insert c [] m) M.empty $ concatMap (zip (take s ['a'..]) . repeat) [1..s]
  , _gsMoves = DL.empty
  , _gsCurrPlayer = Player1
  , _gsSupplyPieces = M.insert Player2 (pieces s) $ M.insert Player1 (pieces s) M.empty
  }
  where
    pieces 3 = (10,0)
    pieces 4 = (15,0)
    pieces 5 = (21,1)
    pieces 6 = (30,1)
    pieces 8 = (50,2)
    pieces _ = error "Can't play a game of this size"

makeMove :: GameState -> Move -> GameState
makeMove gs m = GameState
  { _gsMoves = DL.snoc (gs^.gsMoves) m
  , _gsBoard = updateBoard gs m
  , _gsCurrPlayer = nextPlayer $ gs^.gsCurrPlayer
  , _gsSupplyPieces = case m of
    (Place Cap _) -> updateSupply _2
    (Place _ _)   -> updateSupply _1
    _             -> gs^.gsSupplyPieces
  }
  where
    updateSupply l = M.update (Just <$> over l pred) (gs^.gsCurrPlayer) (gs^.gsSupplyPieces)

updateBoard :: GameState -> Move -> Map Coord [(Player,PieceType)]
updateBoard gs (Place pT c) = M.update (const (Just [(gs^.gsCurrPlayer, pT)])) c $ gs^.gsBoard
updateBoard gs (Move i c d xs) = go (gs^.gsBoard) i c xs
  where
    go m _ _ [] = m
    go m 1 c' [1]
      | smashPossible = smashMove
      | otherwise = normalMove m 1 c' [1]
      where
        nC = goDirection d c'
        p = head $ m M.! c'
        smashPossible = snd p == Cap
        removeCap = M.update (Just <$> tail) c' m
        smashMove = M.update (Just <$> (:) p . over (ix 0 . _2) flattenWall) nC removeCap
        flattenWall Standing = Flat
        flattenWall s = s
    go m i' c' (x':xs') = normalMove m i' c' (x':xs')
    normalMove m i' c' (x':xs') = go nM (i' - x') nC xs'
      where
        nC = goDirection d c'
        pcs = take i' $ m M.! c'
        removePcs = M.update (Just <$> drop i') c' m
        nM = M.update (Just <$> (++) pcs) nC removePcs
updateBoard gs _ = gs^.gsBoard

g1 = makeMove (initialGameState 4) (Place Flat ('a',1))
g2 = makeMove g1 (Place Flat ('a',2))
g3 = makeMove g2 (Move 1 ('a',1) U [1])
g4 = makeMove g3 (Place Flat ('a',1))
g5 = makeMove g4 (Move 2 ('a',2) D [2])
g6 = makeMove g5 (Place Flat ('a',2))
g7 = makeMove g6 (Move 3 ('a',1) U [2,1])
g8 = makeMove g7 (Place Standing ('b',1))
g9 = makeMove g8 (Place Cap ('b',2))
g10 = makeMove g9 (Place Flat ('c',1))
g11 = makeMove g10 (Move 1 ('b',2) D [1])
