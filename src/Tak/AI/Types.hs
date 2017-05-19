{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Tak.AI.Types where

import           AI.Gametree
import           Control.Lens
import           Tak.Types

data AIGameState = AIGameState
    { _aiGameState   :: GameState
    , _aiHumanPlayer :: Player
    , _aiAi          :: AIGameState -> IO Move
    }
makeLenses ''AIGameState


instance Transitions AIGameState Move where
  actions = moves . view aiGameState
  transition m = over aiGameState (`makeMove` m)
