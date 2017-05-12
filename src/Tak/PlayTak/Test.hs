{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Tak.PlayTak.Test where

import           Control.Concurrent.Async
import           Control.Lens
import           Data.ByteString          (ByteString, pack)
import           Data.Monoid
import           Data.Text                (Text)
import           Data.Text.Encoding       (encodeUtf8)
import           Pipes
import qualified Pipes.ByteString         as PB
import           Pipes.Concurrent
import           Pipes.Extras             (delay)
import           Pipes.Group              (concats)
import           Pipes.Network.TCP
import           Pipes.Parse
import qualified Pipes.Prelude            as PP
import           System.Mem
import           Tak.PlayTak.Parser
import           Tak.PlayTak.Types
import           Tak.Types
import qualified Text.Trifecta            as Tri

f :: IO ()
f = do
    (output, input) <- spawn unbounded
    connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
        let
            stdinP = PB.stdin >-> toOutput output
            pinger = Pipes.each (repeat Ping) >-> delay 15 >-> PP.map (encodeUtf8 . (<> "\n") . showCommand) >-> toOutput output
            parser = runParser (fromSocket connectionSocket 4096) >-> PP.print
            toSock = fromInput input >-> toSocket connectionSocket
        mapConcurrently runEffect [pinger,parser,toSock,stdinP]
        return ()

runParser :: (MonadIO m) => Producer ByteString m () -> Producer [PlayTakMessage] m ()
runParser p = do
    (x, p') <- lift (runStateT parseMsg p)
    let isSuccess (Tri.Success _) = True
        isSuccess _ = False
        successes =  filter isSuccess x
        failures = filter (not . isSuccess) x
    mapM_ (liftIO . print) failures
    yield $ map (\(Tri.Success s) -> s) successes
    runParser p'

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
