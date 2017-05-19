module Tak.AI.Random where

import           Control.Lens
import           System.Random
import           Tak.AI.Types
import           Tak.Types

ai :: AIGameState -> IO Move
ai gs = do
    let validMoves = moves (gs^.aiGameState)
    m <- randomRIO (0,length validMoves - 1)
    return $ validMoves !! m
