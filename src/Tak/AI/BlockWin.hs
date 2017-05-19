{-# LANGUAGE ScopedTypeVariables #-}

module Tak.AI.BlockWin where

import           Control.Lens
import qualified Data.DList                    as DL
import           Data.Maybe
import           Data.Tree.Game_tree.Game_tree
import           Data.Tree.Game_tree.Negascout
import           Tak.AI.Types
import           Tak.Parser.PTN
import           Tak.Types
import           Text.Trifecta

newtype BlockWinAI = BlockWinAI { unBlockWinAI :: AIGameState}
instance Game_tree BlockWinAI where
    is_terminal = isJust . view (aiGameState . gsGameOverState) . unBlockWinAI
    node_value = score . unBlockWinAI
    children a = foldMap (\m -> [BlockWinAI $ over aiGameState (`makeMove` m) aigs]) $ moves $ view aiGameState aigs
        where aigs = unBlockWinAI a

score :: AIGameState -> Int
score aigs = if humanTurn then -go else go
    where
        go
            | humanWin = minBound + 4
            | aiWin = maxBound - 3
            | otherwise = 0
        humanTurn = aigs ^. aiGameState . gsCurrPlayer == human
        human = aigs ^. aiHumanPlayer
        humanWin = isWinner human
        aiWin = isWinner (nextPlayer human)
        isWinner w = o == Just (RoadWin w) || o == Just (FlatWin w)
            where o = aigs ^. aiGameState . gsGameOverState

ai :: AIGameState -> IO Move
ai aigs = do
    let (_:a:_, s) = negascout (BlockWinAI aigs) 5
        mvs = DL.toList $ unBlockWinAI a ^. aiGameState . gsMoves
    print (mvs,s)
    return $ last mvs

g = AIGameState (initialGameState 3) Player2 ai

t = parseString gameParser mempty "[Site \"PlayTak.com\"]\n[Date \"2017.5.19\"]\n[Player1 \"Guest3677\"]\n[Player2 \"Guest3680\"]\n[Size \"3\"]\n[Result \"R-0\"]\n1. a1 a2\n2. b2"
game = foldl makeMove (initialGameState 3) $ t^?!_Success._2^..folded.folded.folded._1._Right
aiGame = AIGameState game Player1 ai
