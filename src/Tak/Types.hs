{-# LANGUAGE TemplateHaskell #-}

module Tak.Types where

import           Control.Lens
import           Data.Char        (ord)
import           Data.DList       (DList)
import qualified Data.DList       as DL
import           Data.Foldable
import           Data.Graph.AStar
import qualified Data.HashSet     as H
import           Data.Map.Strict  (Map)
import qualified Data.Map.Strict  as M
import           Data.Maybe
import           Safe (headMay)
import Data.Bool (bool)

type File = Char
type Rank = Int
type Coord = (File, Rank)

isAdjacent :: Coord -> Coord -> Bool
isAdjacent c1 c2 = any ((==) c2 . flip goDirection c1) [L,R,U,D]

manhattenDist :: Coord -> Coord -> Int
manhattenDist (x1,y1) (x2,y2) = abs (ord x1 - ord x2) + abs (y1-y2)

type BoardSize = Int
type BoardState = Map Coord [(Player,PieceType)]
type PieceSupply = Map Player (Int, Int)

data Player = Player1 | Player2
  deriving (Eq, Show, Ord)

nextPlayer :: Player -> Player
nextPlayer Player1 = Player2
nextPlayer Player2 = Player1

data Direction = L | R | U | D
  deriving (Eq, Show)

goDirection :: Direction -> Coord -> Coord
goDirection U = over _2 succ
goDirection D = over _2 pred
goDirection R = over _1 succ
goDirection L = over _1 pred

data PieceType = Flat | Standing | Cap
  deriving (Eq, Show)

data Move = Place PieceType Coord
          | Move Int Coord Direction [Int]
          | Resign
  deriving (Eq, Show)

data GameOverState = RoadWin Player
                    | FlatWin Player
                    | ResignWin Player
                    | Draw
  deriving (Eq,Show)

data GameState = GameState
  { _gsBoard         :: BoardState
  , _gsMoves         :: DList Move
  , _gsCurrPlayer    :: Player
  , _gsSupplyPieces  :: PieceSupply
  , _gsGameOverState :: Maybe GameOverState
  , _gsBoardSize     :: BoardSize
  }
  deriving (Eq,Show)
makeLenses ''GameState

nonStanding :: Player -> BoardState -> BoardState
nonStanding p = M.filter (maybe False isPlayerStanding . headMay)
  where isPlayerStanding (pl,pT) = pT /= Standing && pl == p

initialGameState :: BoardSize -> GameState
initialGameState s = GameState
  { _gsBoard = foldl' (\m c -> M.insert c [] m) M.empty
               $ concatMap (zip (take s ['a'..]) . repeat) [1..s]
  , _gsMoves = DL.empty
  , _gsCurrPlayer = Player1
  , _gsSupplyPieces = M.insert Player2 (pieces s) $ M.insert Player1 (pieces s) M.empty
  , _gsGameOverState = Nothing
  , _gsBoardSize = s
  }
  where
    pieces 3 = (10,0)
    pieces 4 = (15,0)
    pieces 5 = (21,1)
    pieces 6 = (30,1)
    pieces 8 = (50,2)
    pieces _ = error "Can't play a game of this size"

makeMove :: GameState -> Move -> GameState
makeMove gs m  = GameState
  { _gsBoardSize = gs^.gsBoardSize
  , _gsMoves = updatedMoves
  , _gsBoard = updatedBoard
  , _gsCurrPlayer = updatedPlayer
  , _gsSupplyPieces = updatedSupply
  , _gsGameOverState = updatedGameOver
  }
  where
    playersMove = if length (DL.toList (gs^.gsMoves)) < 2
                  then nextPlayer (gs^.gsCurrPlayer)
                  else gs^.gsCurrPlayer
    updatedMoves = DL.snoc (gs^.gsMoves) m
    updatedPlayer = nextPlayer $ gs^.gsCurrPlayer
    updatedBoard = updateBoard playersMove (gs^.gsBoard) m
    updatedSupply = case m of
      (Place Cap _) -> updateSupply _2
      (Place _ _)   -> updateSupply _1
      _             -> gs^.gsSupplyPieces
    updateSupply l = M.update (Just . over l pred) playersMove (gs^.gsSupplyPieces)
    updatedGameOver = case m of
      Resign  -> Just (ResignWin updatedPlayer)
      _ -> case roadWon of
        Nothing -> flatWin updatedSupply updatedBoard
        _ -> roadWon
    roadWon = roadWin (gs^.gsCurrPlayer) (gs^.gsBoardSize) updatedBoard

road :: Player -> BoardSize -> BoardState -> Maybe [Coord]
road p s b = headMay $ mapMaybe shortestPath edgePairs
  where
    shortestPath (c1,c2) = (:) c1 <$> aStar adjacents manhattenDist (const 1) (== c2) c1
    coords = M.keys $ nonStanding p b
    adjacents c = H.fromList $ filter (isAdjacent c) coords
    edgePairs =  [(x,y) | x <- firstRank, y <- lastRank]
              ++ [(x,y) | x <- firstFile, y <- lastFile]
    firstRank = filter ((==) 1 . snd) coords
    lastRank = filter ((==) s . snd) coords
    firstFile = filter ((==) 'a' . fst) coords
    lastFile = filter ((==) lastFileChr . fst) coords
    lastFileChr = last $ take s ['a'..]

roadWin :: Player -> BoardSize -> BoardState -> Maybe GameOverState
roadWin p s bs
  | doubleRoad    = Just (RoadWin p)
  | isJust p1Road = Just (RoadWin Player1)
  | isJust p2Road = Just (RoadWin Player2)
  | otherwise     = Nothing
  where
    doubleRoad = isJust p1Road && isJust p2Road
    p1Road = road Player1 s bs
    p2Road = road Player2 s bs

flatWin :: PieceSupply -> BoardState -> Maybe GameOverState
flatWin ps bs
  | noneInSupply = case compare p1Count p2Count of
                   GT -> Just (FlatWin Player1)
                   LT -> Just (FlatWin Player2)
                   EQ -> Just Draw
  | otherwise = Nothing
  where
    p1Count = M.size $ nonStanding Player1 bs
    p2Count = M.size $ nonStanding  Player2 bs
    noneInSupply = elem 0 $ fmap (uncurry (+)) ps

updateBoard :: Player -> BoardState -> Move -> BoardState
updateBoard player bs (Place pT c) = M.insert c [(player, pT)] bs
updateBoard _ bs (Move i c d xs) = go bs i c xs
  where
    go m _ _ [] = m
    go m i' c' (x':xs') = go newBoard (i' - x') newCoord xs'
      where
        newCoord = goDirection d c'
        pcs@(p:_) = take i' $ m M.! c'
        removePcs = M.update (Just . drop i') c' m
        newBoard = M.update (Just . (++) pcs . smashIfNeeded) newCoord removePcs
        smashPossible = i' == 1 && null xs' && x' == 1 && snd p == Cap
        smashIfNeeded
          | smashPossible = over (ix 0 . _2) flattenWall
          | otherwise = id
        flattenWall Standing = Flat
        flattenWall s = s
updateBoard _ bs _ = bs

moves :: GameState -> [Move]
moves gs = placeMoves ++ moveMoves
  where
    firstTwoTurns = length (DL.toList (gs^.gsMoves)) < 2
    validPieceTypes 
      | firstTwoTurns = [Flat]
      | otherwise = (\(r,c) -> bool [Flat,Standing] []  (r==0) 
                            ++ bool [Cap] [] (c==0))
                    $ (gs^.gsSupplyPieces) M.! (gs^.gsCurrPlayer)
    emptySpaces = M.keys $ M.filter null (gs^.gsBoard)                      
    placeMoves = [Place pT c | pT <- validPieceTypes, c <- emptySpaces]
    playerStacks = M.filter (maybe False ((==) (gs^.gsCurrPlayer) . fst) . headMay) (gs^.gsBoard)
    moveInDirection :: Direction -> (Coord, [(Player,PieceType)]) -> [Move]
    moveInDirection d (c, pcs) = concatMap goMove takes
      where
        takes = [1..(min (gs^.gsBoardSize) (length pcs))]
        goMove i =  map (Move i c d) $ go c $ take i (map snd pcs)
        go :: Coord -> [PieceType] -> [[Int]]
        go _ [] = []
        go c' ps@(p:_)
          | canGoInDir = map (\i -> concatMap ((:) i) (go nextCoord (dropFromEnd i ps))) [1..length ps]
          | otherwise = []
          where
            canGoInDir = isEmpty || isPieceType Flat || canSmash
            isEmpty = maybe False null nextSpot
            isPieceType pT = maybe False ((==) pT . snd) (nextSpot >>= headMay)
            canSmash = length ps == 1 && p == Cap && isPieceType Standing
            dropFromEnd i = reverse . drop i . reverse
            nextSpot = gs^.gsBoard.at nextCoord
            nextCoord = goDirection d c'
    moveMoves 
      | firstTwoTurns = []
      | otherwise     = concat $ M.elems $ 
        M.mapWithKey (\c mv ->concat $ zipWith moveInDirection [D,U,L,R] $ repeat (c,mv)) playerStacks

g1 = makeMove (initialGameState 4) (Place Flat ('a',2))
g2 = makeMove g1 (Place Flat ('a',1))
g3 = makeMove g2 (Move 1 ('a',1) U [1])
g4 = makeMove g3 (Place Flat ('a',1))
g5 = makeMove g4 (Move 2 ('a',2) D [2])
g6 = makeMove g5 (Place Flat ('a',2))
g7 = makeMove g6 (Move 3 ('a',1) U [2,1])
g8 = makeMove g7 (Place Standing ('b',1))
g9 = makeMove g8 (Place Cap ('b',2))
g10 = makeMove g9 (Place Flat ('c',1))
g11 = makeMove g10 (Move 1 ('b',2) D [1])
g12 = makeMove g11 (Place Flat ('d',4))

w1 = makeMove (initialGameState 4) (Place Flat ('a',1))
w2 = makeMove w1 (Place Flat ('b',1))
w3 = makeMove w2 (Place Flat ('a',2))
w4 = makeMove w3 (Place Flat ('b',2))
w5 = makeMove w4 (Place Flat ('a',3))
w6 = makeMove w5 (Place Flat ('b',3))
w7 = makeMove w6 (Place Cap ('a',4))
