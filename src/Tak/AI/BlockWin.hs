{-# LANGUAGE ScopedTypeVariables #-}

module Tak.AI.BlockWin where

import           AI.Minimax
import           Control.Lens
import           Data.Maybe
import           Tak.AI.Types
import           Tak.Types

score :: AIGameState -> Value
score aigs
    | isJust (aigs ^. aiGameState . gsGameOverState) = maxBound-1
    | otherwise = 0
    where
        human = aigs ^. aiHumanPlayer
        humanWin = isWinner human
        aiWin = isWinner (nextPlayer human)
        isWinner w = o == Just (RoadWin w) || o == Just (FlatWin w)
            where o = aigs ^. aiGameState . gsGameOverState

ai :: AIGameState -> IO Move
ai aigs = do
    let m = searchMove (alphaBeta score) 7 aigs
    print m
    return $ fst m
