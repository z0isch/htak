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

pieceSupplies :: Getter GameState [PieceSupply]
pieceSupplies = to (\gs -> [gs^.gsPlayer1Supply, gs^.gsPlayer1Supply])

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
makeMove gs m  = newState
  where
    newState = gs &
      set gsBoard updatedBoard &
      set gsGameOverState updatedGameOver &
      over currPlayerSupply updateSupply &
      over gsMoves (`DL.snoc` m) &
      over gsCurrPlayer nextPlayer
    playersMove = if isFirstTwoTurns gs
                  then nextPlayer (gs^.gsCurrPlayer)
                  else gs^.gsCurrPlayer
    updatedBoard = updateBoard playersMove (gs^.gsBoard) m
    updateSupply (r,c) = case m of
      (Place Cap _) -> (r,c-1)
      (Place _ _)   -> (r-1,c)
      _             -> (r,c)
    updatedGameOver = case m of
      Resign  -> Just (ResignWin (newState^.gsCurrPlayer))
      _ -> case roadWon of
        Nothing -> flatWin (newState^.pieceSupplies) (newState^.gsBoard)
        _ -> roadWon
    roadWon = roadWin (gs^.gsCurrPlayer) (newState^.gsBoardSize) (newState^.gsBoard)

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
  | otherwise          = concatMap mkMoves $ validCoordsToMove gs
  where 
    mkMoves (c,d) = map (\i -> Move (sum i) c d i) (drops gs (c,d))

validCoordsToMove :: GameState -> [(Coord,Direction)]
validCoordsToMove gs = concatMap (\c -> map (\d -> (c,d)) (filter (canGoDir c) [U,D,L,R])) playerCoords
  where
    playerCoords = M.keys $ M.filter topIsPlayers (gs^.gsBoard)
    topIsPlayers = maybe False ((==) (gs^.gsCurrPlayer) . fst) . headMay      
    canGoDir c d = isValidRegularMoveTo gs (goDirection d c) || canSmash (gs^.gsBoard.at c)
      where
        canSmash (Just ((_,Cap):[])) = isTopPieceType gs (goDirection d c) Standing
        canSmash _ = False

drops :: GameState -> (Coord, Direction) -> [[Int]]
drops gs (c,d) = concatMap (go c . flip take pts) [1..min (gs^.gsBoardSize) (length pts)]
  where
    pts = map snd $ (gs^.gsBoard) M.! c
    go :: Coord -> [PieceType] -> [[Int]]
    go _ [] = [[]]
    go c' ps@(p:_)
      | notValidCoord = []
      | canGoFurther  = concatMap (\i -> map ((:) i) (nextLevel i))  [1..length ps]
      | otherwise     = [[length ps]]
      where
        notValidCoord = not (M.member nextCoord (gs^.gsBoard))
        nextLevel i = go nextCoord $ take (length ps - i) ps
        canGoFurther = isValidRegularMoveTo gs nextCoord || canSmash 
        canSmash = length ps == 1 && p == Cap && isTopPieceType gs nextCoord Standing
        nextCoord = goDirection d c'

isValidRegularMoveTo :: GameState -> Coord -> Bool
isValidRegularMoveTo gs c = isEmpty || isTopPieceType gs c Flat
  where isEmpty = maybe False null $ gs^.gsBoard.at c

isTopPieceType :: GameState -> Coord -> PieceType -> Bool
isTopPieceType gs c pT = maybe False ((==) pT . snd) $ gs^.gsBoard.at c >>= headMay

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
