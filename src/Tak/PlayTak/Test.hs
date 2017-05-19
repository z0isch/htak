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
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           Pipes
import qualified Pipes.ByteString         as PB
import           Pipes.Concurrent
import           Pipes.Extras             (delay)
import           Pipes.Network.TCP
import qualified Pipes.Prelude            as PP
import           Tak.AI.BlockWin
import           Tak.AI.Types
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
    (cmdOutput, cmdInput) <- spawn unbounded
    connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
        let stdinP = PB.stdin >-> PP.map PlainText >-> toOutput cmdOutput
            pinger = each (repeat Ping) >-> delay 15 >-> toOutput cmdOutput
            parser = runParser (fromSocket connectionSocket 4096)
                        >-> PP.tee PP.print
                        >-> forever (await >>= each)
            botRunner = parser
                        >-> botStatePipe cmdOutput
                        >-> toOutput cmdOutput
            toSock = fromInput cmdInput >-> PP.map commandToBS >-> toSocket connectionSocket
        mapConcurrently_ runEffect [toSock,pinger,botRunner,stdinP]

botStatePipe :: Output PlayTakCommand -> Pipe PlayTakMessage PlayTakCommand IO ()
botStatePipe output = go initialBotState
    where
        initialBotState = BotState Nothing M.empty output
        go bs = do
            (bs',cmd) <- await >>= lift . runMsg bs
            maybe (return ()) yield cmd
            go bs'

initialSeek :: PlayTakCommand
initialSeek = Seek 3 600 600 Nothing

runMsg :: BotState -> PlayTakMessage -> IO (BotState, Maybe PlayTakCommand)
runMsg bs LoginOrRegister = return (bs, Just LoginGuest)
runMsg bs (Welcome (Just a) )= return (set bsName (Just a) bs, Just initialSeek)
runMsg bs (GameMsgStart gameNum size p1 p2 p) = do
    aiState <- newMVar initAIState
    when isAITurn $ modifyStateAsync aiState $ makeAndSendAIMove (bs^.bsCmdOutput) gameNum
    return (over bsAIState (M.insert gameNum aiState) bs, Nothing)
    where
        initAIState = AIGameState (initialGameState size) (nextPlayer p) ai
        isAITurn = initAIState^.aiGameState.gsCurrPlayer /= initAIState^.aiHumanPlayer
runMsg bs (GameMsgMove gameNum m) = do
    modifyStateAsync aiState $
        makeAndSendAIMove (bs^.bsCmdOutput) gameNum . over aiGameState (`makeMove` m)
    return (bs, Nothing)
    where
        (Just aiState) = bs^.bsAIState.at gameNum
runMsg bs (GameMsgOver _ _) = return (bs, Just initialSeek)
runMsg bs (GameMsgAbandoned gameNum) = do
     modifyStateAsync aiState $ return . over aiGameState (`makeMove` Resign)
     return (bs,Just initialSeek)
     where
        (Just aiState) = bs^.bsAIState.at gameNum
runMsg bs _ = return (bs, Nothing)

modifyStateAsync :: MVar a -> (a -> IO a) -> IO ()
modifyStateAsync s = void . async . modifyMVar_ s

makeAndSendAIMove :: Output PlayTakCommand -> GameNumber -> AIGameState -> IO AIGameState
makeAndSendAIMove output gameNum s = do
    m <- (s^.aiAi) s
    runEffect $ yield (GameCmdMove gameNum m) >-> toOutput output
    return $ over aiGameState (`makeMove` m) s


