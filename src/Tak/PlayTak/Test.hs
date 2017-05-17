{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

module Tak.PlayTak.Test where

import           Control.Concurrent.Async
import           Control.Concurrent.MVar
import           Control.Lens             (at, makeLenses, over, set, view,
                                           (^.))
import           Control.Monad
import           Data.ByteString          (ByteString)
import           Data.Map.Strict          (Map)
import qualified Data.Map.Strict          as M
import           Data.Maybe
import           Data.Monoid
import           Data.Text                (Text, pack)
import           Data.Text.Encoding       (encodeUtf8)
import           Pipes
import qualified Pipes.ByteString         as PB
import           Pipes.Concurrent
import           Pipes.Extras             (delay)
import           Pipes.Group              (concats)
import           Pipes.Network.TCP
import qualified Pipes.Prelude            as PP
import           Tak.AI.Random
import           Tak.AIGame
import           Tak.PlayTak.Parser
import           Tak.PlayTak.Printer
import           Tak.PlayTak.Types
import           Tak.Types

data BotState = BotState
    { _bsName    :: Maybe Text
    , _bsAIState :: Map GameNumber (MVar AIGameState)
    }
makeLenses ''BotState

commandToBS :: PlayTakCommand -> ByteString
commandToBS = encodeUtf8 . (<> "\n") . showCommand

f :: IO ()
f = do
    (output, input) <- spawn unbounded
    connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
        let
            stdinP = PB.stdin >-> toOutput output
            pinger = each (repeat "PING\n") >-> delay 15 >-> toOutput output
            parser = runParser (fromSocket connectionSocket 4096)
                >-> PP.tee PP.print
                >-> forever (await >>= each)
                >-> botStatePipe output
                >-> PP.map commandToBS
                >-> toOutput output
            toSock = fromInput input >-> toSocket connectionSocket
        mapConcurrently_ runEffect [pinger,parser,toSock,stdinP]

linesOfSocket :: Socket -> Producer PB.ByteString IO ()
linesOfSocket s = concats (fromSocket s 4096 ^. PB.lines)

botStatePipe :: Output ByteString -> Pipe PlayTakMessage PlayTakCommand IO ()
botStatePipe output = go initialBotState
    where
        initialBotState = BotState Nothing M.empty
        initialSeek = Seek 3 600 600 (Just Player1)
        go bs = do
            msg <- await
            (bs',cmd) <- lift $ runMsg bs msg
            maybe (return ()) yield cmd
            go bs'
        runMsg :: BotState -> PlayTakMessage -> IO (BotState, Maybe PlayTakCommand)
        runMsg bs LoginOrRegister = return (bs, Just LoginGuest)
        runMsg bs (Welcome (Just a) )= return (set bsName (Just a) bs, Just initialSeek)
        runMsg bs (GameMsgStart gameNum size p1 p2 p) = do
            aiState <- newMVar $ AIGameState (initialGameState size) (nextPlayer p) ai
            let bs' = over bsAIState (M.insert gameNum aiState) bs
            modifyState aiState $ \s ->
                if s^.aiGameState.gsCurrPlayer == s^.aiHumanPlayer
                then return s
                else makeAIMove gameNum s
            return (bs', Nothing)
        runMsg bs (GameMsgMove gameNum m) = do
            let (Just aiState) = bs^.bsAIState.at gameNum
            modifyState aiState $ makeAIMove gameNum . over aiGameState (`makeMove` m)
            withMVar aiState (print . view (aiGameState.gsBoard))
            return (bs, Nothing)
        runMsg bs _ = return (bs, Nothing)
        modifyState s = void . async . modifyMVar_ s
        makeAIMove gameNum s = do
            m <- (s^.aiAi) (s^.aiGameState)
            runEffect $ yield (GameCmdMove gameNum m) >-> PP.map commandToBS >-> toOutput output
            return $ over aiGameState (`makeMove` m) s


