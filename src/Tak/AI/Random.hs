module Tak.AI.Random where

import           System.Random
import           Tak.Types

ai :: GameState -> IO Move
ai gs = do
    let validMoves = moves gs
    m <- randomRIO (0,length validMoves - 1)
    return $ validMoves !! m
