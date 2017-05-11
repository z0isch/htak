{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tak.PlayTak.Parser where

import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Text             (pack)
import qualified Pipes.Parse           as PP
import           Tak.PlayTak.Types
import           Text.Trifecta

-- This will keep drawing and parsing from the pipe until it finds one that ends in a \n
parseMsg :: (Monad m) => PP.Parser ByteString m [Result PlayTakMessage]
parseMsg = go initialParse
    where
        initialParse = stepParser (release mempty *> playTakMessageParser) mempty mempty
        go s = do
            mI <- PP.draw
            case mI of
                Nothing -> go s
                Just i -> if BS.null i
                    then go s
                    else do
                        let xs = BC.split '\n' i
                            parseAll = init xs
                            partialParse = last xs
                            parsed = zipWith runParser parseAll (s:repeat initialParse)
                            runParser i = starve . feed i
                            partialStep = if null parseAll then s else initialParse
                        if BS.null partialParse
                        then return parsed
                        else (++) parsed <$> go (feed i partialStep)

playTakMessageParser :: Parser PlayTakMessage
playTakMessageParser = choice parsers
    where
        parsers = [welcome,login,online,ok,nok]
        welcome = Welcome <$> (text "Welcome" *> (optional nameParser <* char '!'))
        nameParser = pack <$> (text " " *> many (noneOf "!"))
        login = LoginOrRegister <$ text "Login or Register"
        online = Online <$> (text "Online " *> natural)
        ok = OK <$ text "OK"
        nok = NOK <$ text "NOK"
