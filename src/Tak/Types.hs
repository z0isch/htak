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
import           Safe             (headMay)

type File = Char
type Rank = Int
type Coord = (File, Rank)

isAdjacent :: Coord -> Coord -> Bool
isAdjacent c1 c2 = any ((==) c2 . flip goDirection c1) [L,R,U,D]

manhattenDist :: Coord -> Coord -> Int
manhattenDist (x1,y1) (x2,y2) = abs (ord x1 - ord x2) + abs (y1-y2)

type BoardSize = Int
type BoardState = Map Coord [(Player,PieceType)]
type PieceSupply = (Int, Int)

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
  , _gsPlayer1Supply :: PieceSupply
  , _gsPlayer2Supply :: PieceSupply
  , _gsGameOverState :: Maybe GameOverState
  , _gsBoardSize     :: BoardSize
  }
  deriving (Eq,Show)
makeLenses ''GameState

otherPlayerSupply :: Lens' GameState PieceSupply
otherPlayerSupply f gs = case gs^.gsCurrPlayer of
    Player1 -> gsPlayer2Supply f gs
    Player2 -> gsPlayer1Supply f gs

currPlayerSupply :: Lens' GameState PieceSupply
currPlayerSupply f gs = case gs^.gsCurrPlayer of
    Player1 -> gsPlayer1Supply f gs
    Player2 -> gsPlayer2Supply f gs

isFirstTwoTurns :: GameState -> Bool
isFirstTwoTurns gs = length (DL.toList (gs^.gsMoves)) < 2

nonStanding :: Player -> BoardState -> BoardState
nonStanding p = M.filter (maybe False isPlayerStanding . headMay)
  where isPlayerStanding (pl,pT) = pT /= Standing && pl == p

initialGameState :: BoardSize -> GameState
initialGameState s = GameState
  { _gsBoard = foldl' (\m c -> M.insert c [] m) M.empty
               $ concatMap (zip (take s ['a'..]) . repeat) [1..s]
  , _gsMoves = DL.empty
  , _gsCurrPlayer = Player1
  , _gsPlayer1Supply = pieces s
  , _gsPlayer2Supply = pieces s
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
makeMove gs m = gs & set gsBoard updatedBoard
                   & set currPlayerSupply updatedSupply
                   & set gsGameOverState updatedGameOver
                   & set gsCurrPlayer nextToPlay
                   & over gsMoves (`DL.snoc` m)
  where
    updatedGameOver = case m of
      Resign  -> Just (ResignWin nextToPlay)
      _ -> case roadWon of
        Nothing -> flatWin [gs^.otherPlayerSupply, updatedSupply] updatedBoard
        _ -> roadWon
    roadWon = roadWin (gs^.gsCurrPlayer) (gs^.gsBoardSize) updatedBoard
    updatedSupply = updateSupply (gs^.currPlayerSupply)
    updateSupply (r,c) = case m of
      (Place Cap _) -> (r,c-1)
      (Place _ _)   -> (r-1,c)
      _             -> (r,c)
    updatedBoard = updateBoard playersMove m (gs^.gsBoard)
    playersMove
      | isFirstTwoTurns gs = nextToPlay
      | otherwise          = gs^.gsCurrPlayer
    nextToPlay = nextPlayer $ gs^.gsCurrPlayer

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

flatWin :: [PieceSupply] -> BoardState -> Maybe GameOverState
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

updateBoard :: Player -> Move -> BoardState -> BoardState
updateBoard player (Place pT c) bs = M.insert c [(player, pT)] bs
updateBoard _ (Move i c d xs) bs = go bs i c xs
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
updateBoard _ _ bs = bs

moves :: GameState -> [Move]
moves gs = placeMoves gs ++ moveMoves gs

placeMoves :: GameState -> [Move]
placeMoves gs = [Place pT c | pT <- validPieceTypes, c <- emptySpaces]
  where
    emptySpaces = M.keys $ M.filter null (gs^.gsBoard)
    validPieceTypes
      | isFirstTwoTurns gs = [Flat]
      | otherwise          = pieceTypes (gs^.currPlayerSupply)
    pieceTypes (0,0) = []
    pieceTypes (_,0) = [Flat,Standing]
    pieceTypes (0,_) = [Cap]
    pieceTypes _     = [Flat,Standing,Cap]

moveMoves :: GameState -> [Move]
moveMoves gs
  | isFirstTwoTurns gs = []
  | otherwise          = concatMap mkAllMoves playerCoords
  where
    mkAllMoves c = concatMap (mkMoves c) [L,R,U,D]
    mkMoves c d = map (\i -> Move (sum i) c d i) (possibleDrops gs c d)
    playerCoords = M.keys $ M.filter topIsPlayers (gs^.gsBoard)
    topIsPlayers = maybe False ((==) (gs^.gsCurrPlayer) . fst) . headMay

possibleDrops :: GameState -> Coord -> Direction -> [[Int]]
possibleDrops gs c d = concatMap (go c) piecesToPickUp
  where
    piecesToPickUp = map (`take` pts) numsToPickUp
    numsToPickUp = [1..min (gs^.gsBoardSize) (length pts)]
    pts = map snd $ (gs^.gsBoard) M.! c
    go _ [] = [[]]
    go c' ps@(p:_)
      | invalidCoord = []
      | canGoFurther  = concatMap nextLevel [1..length ps]
      | otherwise     = [[length ps]]
      where
        invalidCoord = M.notMember nextCoord (gs^.gsBoard)
        nextLevel i = map (i:) $ go nextCoord $ take (length ps - i) ps
        canGoFurther = nextEmpty || nextTopPieceType Flat || canSmash
        canSmash = length ps == 1 && p == Cap && nextTopPieceType Standing
        nextEmpty = maybe False null $ gs^.gsBoard.at nextCoord
        nextTopPieceType pT = maybe False ((==) pT . snd) $ gs^.gsBoard.at nextCoord >>= headMay
        nextCoord = goDirection d c'
