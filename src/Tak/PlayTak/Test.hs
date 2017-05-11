{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Tak.PlayTak.Test where

import           Control.Concurrent.Async
import           Control.Lens
import           Data.ByteString          (ByteString)
import           Data.Text                (Text)
import           Pipes
import qualified Pipes.ByteString         as PB
import           Pipes.Group              (concats)
import           Pipes.Network.TCP
import           Pipes.Parse
import qualified Pipes.Prelude            as PP
import           Tak.PlayTak.Parser
import           Tak.PlayTak.Types
import           Tak.Types
import qualified Text.Trifecta            as Tri

f :: IO ()
f = connect "playtak.com" "10000" $ \(connectionSocket,_) -> do
    let act1 = runEffect $ PB.stdin >-> toSocket connectionSocket
        act2 = runEffect $ runParser (fromSocket connectionSocket 4096) >-> PP.print
    concurrently act1 act2
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
showCommand _ = ""
