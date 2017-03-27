{-# LANGUAGE TemplateHaskell #-}

module Tak.AIGame where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.State
import           Data.Maybe
import           Safe                         (readMay)
import qualified Tak.AI.Random                as Random
import           Tak.Parser.PTN
import           Tak.Printer.PTN
import           Tak.Types
import           Text.PrettyPrint.ANSI.Leijen (putDoc)
import           Text.Trifecta

data AIGameState = AIGameState
    { _aiGameState   :: GameState
    , _aiHumanPlayer :: Player
    , _aiAi          :: GameState -> IO Move
    }
makeLenses ''AIGameState

runAiGame :: IO ()
runAiGame = do
    s <- AIGameState <$> initializeGameState <*> blackOrWhite <*> chooseAI
    win <- execStateT aiLoop s
    print $ win^.aiGameState.gsGameOverState

initializeGameState :: IO GameState
initializeGameState = do
    size <- putStr "What size board (3-8)? " >> getLine
    let parsed= do
            i <- readMay size
            if i >= 3 && i <= 8 then Just i else Nothing
    case parsed of
        Nothing -> putStrLn "- Please input a number 3-8" >> initializeGameState
        Just i -> return (initialGameState i)

blackOrWhite :: IO Player
blackOrWhite = do
    p <- putStr "(w)hite or (b)lack? " >> getLine
    if p == "w" then return Player1
    else
        if p == "b" then return Player2
        else putStrLn "- Please input w or b" >> blackOrWhite

chooseAI :: IO (GameState -> IO Move)
chooseAI = do
    p <- putStr "Choose AI: (1) Random " >> getLine
    if p == "1" then return $ Random.ai
    else putStrLn "- Please input 1" >> chooseAI

getPlayersMove :: GameState -> IO Move
getPlayersMove gs = do
    mS <- putStr "Move? " >> getLine
    case parseString moveParser mempty mS of
        Failure d -> putDoc d >> putStrLn "" >> getPlayersMove gs
        Success m -> if m `elem` moves gs
                     then return m
                     else do
                        _ <- liftIO $ putStrLn $ "Can't make this move: " ++ show m
                        getPlayersMove gs

aiLoop :: StateT AIGameState IO ()
aiLoop = do
    s <- get
    let isPlayersTurn = s^.aiGameState.gsCurrPlayer == s^.aiHumanPlayer
    m <- if isPlayersTurn
         then liftIO $ getPlayersMove (s^.aiGameState)
         else liftIO $ (s^.aiAi) (s^.aiGameState)
    aiGameState %= (`makeMove` m)
    unless isPlayersTurn $ liftIO $ putStrLn $ "- AI: " ++ printMove (Right m)
    gs <- use aiGameState
    liftIO $ print gs >> putStrLn ""
    when (isNothing $ gs^.gsGameOverState) aiLoop
