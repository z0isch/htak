{-# LANGUAGE TemplateHaskell #-}

module Tak.AIGame where

import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.State
import           Data.Maybe
import           Safe                         (readMay)
import           System.Random
import           Tak.Parser.PTN
import           Tak.Printer.PTN
import           Tak.Types
import           Text.PrettyPrint.ANSI.Leijen (putDoc)
import           Text.Trifecta

type AI = GameState -> IO Move
data AIGameState = AIGameState
    { _aiGameState   :: GameState
    , _aiHumanPlayer :: Player
    , _aiAi          :: AI
    }
makeLenses ''AIGameState

type AIGame = StateT AIGameState IO

randomAi :: GameState -> IO Move
randomAi gs = do
    let validMoves = moves gs
    m <- randomRIO (0,length validMoves - 1)
    return $ validMoves !! m

runAiGame :: IO ()
runAiGame = do
    gs <- initializeGameState
    p <- blackOrWhite
    _ <- execStateT aiGame $ AIGameState gs p randomAi
    return ()

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

getPlayersMove :: AIGame Move
getPlayersMove = do
    aiGs <- get
    mS <- liftIO (putStr "Move? ") >> liftIO getLine
    case parseString moveParser mempty mS of
        Failure d -> liftIO (putDoc d >> putStrLn "") >> getPlayersMove
        Success m -> if m `elem` moves (aiGs^.aiGameState)
                     then return m
                     else do
                        _ <- liftIO $ putStrLn $ "Can't make this move: " ++ show m
                        getPlayersMove

aiGame :: AIGame ()
aiGame = do
    aiLoop
    s <- get
    liftIO $ print $ s^.aiGameState.gsGameOverState

aiLoop :: AIGame ()
aiLoop = do
    s <- get
    let isPlayersTurn = s^.aiGameState.gsCurrPlayer == s^.aiHumanPlayer
    m <- if isPlayersTurn
         then getPlayersMove
         else liftIO $ (s^.aiAi) (s^.aiGameState)
    aiGameState %= (`makeMove` m)
    s <- get
    unless isPlayersTurn $ liftIO $ putStrLn $ "- AI: " ++ printMove (Right m)
    liftIO $ print (s^.aiGameState) >> putStrLn ""
    unless (isJust $ s^.aiGameState.gsGameOverState) aiLoop
