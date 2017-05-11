{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module Tak.PlayTak.Test where

import           Control.Concurrent.Async
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
        act2 = runEffect $ playTakPipeParse connectionSocket >-> PP.print
    concurrently act1 act2
    return ()


playTakPipeParse :: Socket -> Producer [PlayTakMessage] IO ()
playTakPipeParse connectionSocket = runParser $ fromSocket connectionSocket 1
  where
--    linesOfSocket :: Producer PB.ByteString IO ()
--    linesOfSocket = concats (fromSocket connectionSocket 4096 ^. PB.lines)
    runParser :: Producer ByteString IO () -> Producer [PlayTakMessage] IO ()
    runParser p = do
        (x, p') <- lift (runStateT parseMsg p)
        let isSuccess (Tri.Success _) = True
            isSuccess _ = False
            successes =  filter isSuccess x
            failures = filter (not . isSuccess) x
        mapM_ (liftIO . print) failures
        yield $ map (\(Tri.Success s) -> s) successes
        runParser p'


data BotState = BotState {}
    deriving (Eq, Show)

runMessage :: BotState -> PlayTakMessage -> (BotState, Maybe PlayTakCommand)
runMessage bs LoginOrRegister = (bs,Just LoginGuest)
runMessage bs _ = (bs,Nothing)

showCommand :: PlayTakCommand -> Text
showCommand LoginGuest = "Login Guest"
showCommand _ = ""
