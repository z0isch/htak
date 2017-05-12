{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tak.PlayTak.Test where

import           Control.Concurrent.Async
import           Control.Lens             ((^.))
import           Data.ByteString          (ByteString)
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           Pipes
import qualified Pipes.ByteString         as PB
import           Pipes.Concurrent
import           Pipes.Extras             (delay)
import           Pipes.Group              (concats)
import           Pipes.Network.TCP
import qualified Pipes.Prelude            as PP
import           Tak.PlayTak.Parser
import           Tak.PlayTak.Types

commandToBS :: PlayTakCommand -> ByteString
commandToBS = encodeUtf8 . (<> "\n") . showCommand

f :: IO ()
f = do
    (output, input) <- spawn unbounded
    connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
        let
            stdinP = PB.stdin >-> toOutput output
            pinger = each (repeat "PING\n") >-> delay 15 >-> toOutput output
            parser = runParser (fromSocket connectionSocket 4096) >-> PP.print
            toSock = fromInput input >-> toSocket connectionSocket
        mapConcurrently_ runEffect [pinger,parser,toSock,stdinP]
        return ()

linesOfSocket :: Socket -> Producer PB.ByteString IO ()
linesOfSocket s = concats (fromSocket s 4096 ^. PB.lines)

data BotState = BotState {}
    deriving (Eq, Show)

runMessage :: BotState -> PlayTakMessage -> (BotState, Maybe PlayTakCommand)
runMessage bs LoginOrRegister = (bs,Just LoginGuest)
runMessage bs _ = (bs,Nothing)

showCommand :: PlayTakCommand -> Text
showCommand LoginGuest = "Login Guest"
showCommand Ping = "PING"
showCommand _ = ""
