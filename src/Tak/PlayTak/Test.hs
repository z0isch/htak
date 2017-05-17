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
    { _bsName      :: Maybe Text
    , _bsAIState   :: Map GameNumber (MVar AIGameState)
    , _bsCmdOutput :: Output PlayTakCommand
    }
makeLenses ''BotState

commandToBS :: PlayTakCommand -> ByteString
commandToBS = encodeUtf8 . (<> "\n") . showCommand

f :: IO ()
f = do
    (output, input) <- spawn unbounded
    connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
        let stdinP = PB.stdin >-> PP.map PlainText >-> toOutput output
            pinger = each (repeat Ping) >-> delay 15 >-> toOutput output
            parser = runParser (fromSocket connectionSocket 4096)
                        >-> PP.tee PP.print
            botRunner = parser
                        >-> forever (await >>= each)
                        >-> botStatePipe output
                        >-> toOutput output
            toSock = fromInput input >-> PP.map commandToBS >-> toSocket connectionSocket
        mapConcurrently_ runEffect [pinger,botRunner,toSock,stdinP]

linesOfSocket :: Socket -> Producer PB.ByteString IO ()
linesOfSocket s = concats (fromSocket s 4096 ^. PB.lines)

botStatePipe :: Output PlayTakCommand -> Pipe PlayTakMessage PlayTakCommand IO ()
botStatePipe output = go initialBotState
    where
        initialBotState = BotState Nothing M.empty output
        go bs = do
            msg <- await
            (bs',cmd) <- lift $ runMsg bs msg
            maybe (return ()) yield cmd
            go bs'

runMsg :: BotState -> PlayTakMessage -> IO (BotState, Maybe PlayTakCommand)
runMsg bs LoginOrRegister = return (bs, Just LoginGuest)
runMsg bs (Welcome (Just a) )= return (set bsName (Just a) bs, Just initialSeek)
    where initialSeek = Seek 3 600 600 (Just Player2)
runMsg bs (GameMsgStart gameNum size p1 p2 p) = do
    let aiState = AIGameState (initialGameState size) (nextPlayer p) ai
        isAITurn = aiState^.aiGameState.gsCurrPlayer /= aiState^.aiHumanPlayer
    aiStateM <- newMVar aiState
    let bs' = over bsAIState (M.insert gameNum aiStateM) bs
    when isAITurn $ modifyStateAsync  aiStateM $ makeAIMove (bs'^.bsCmdOutput) gameNum
    return (bs', Nothing)
runMsg bs (GameMsgMove gameNum m) = do
    let (Just aiState) = bs^.bsAIState.at gameNum
    modifyStateAsync aiState $ makeAIMove (bs^.bsCmdOutput) gameNum . over aiGameState (`makeMove` m)
    withMVar aiState (print . view (aiGameState.gsBoard))
    return (bs, Nothing)
runMsg bs _ = return (bs, Nothing)

modifyStateAsync :: MVar a -> (a -> IO a) -> IO ()
modifyStateAsync s = void . async . modifyMVar_ s

makeAIMove :: Output PlayTakCommand -> GameNumber -> AIGameState -> IO AIGameState
makeAIMove output gameNum s = do
    m <- (s^.aiAi) (s^.aiGameState)
    runEffect $ yield (GameCmdMove gameNum m) >-> toOutput output
    return $ over aiGameState (`makeMove` m) s


