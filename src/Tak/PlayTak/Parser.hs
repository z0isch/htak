{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Tak.PlayTak.Parser where

import           Control.Applicative
import           Data.ByteString       (ByteString)
import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import           Data.Char             (toLower)
import           Data.Text             (pack)
import           Pipes
import qualified Pipes.Parse           as PP
import           Tak.Parser.PTN        (gameOverParser)
import           Tak.PlayTak.Types
import           Tak.Types
import           Text.Trifecta

-- Prints out parsing errors, returns successful parses
runParser :: Producer ByteString IO () -> Producer [PlayTakMessage] IO ()
runParser p = do
    (x, p') <- lift (PP.runStateT parseMsg p)
    let isSuccess (Success _) = True
        isSuccess _ = False
        successes =  filter isSuccess x
        failures = filter (not . isSuccess) x
    mapM_ (lift . print) failures
    yield $ map (\(Success s) -> s) successes
    runParser p'

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
                            parsed = zipWith runP parseAll (s:repeat initialParse)
                            runP x = starve . feed x
                            partialStep = if null parseAll then s else initialParse
                        if BS.null partialParse
                        then return parsed
                        else (++) parsed <$> go (feed i partialStep)

playTakMessageParser :: Parser PlayTakMessage
playTakMessageParser = choice $ map try parsers
    where
        parsers = [welcome,login,online,ok,nok, seekNew, seekRemove, gameListAdd, gameListRemove, gameStart, gameMove, gamePlace, gameOver, gameAbandoned]
        welcome = Welcome <$> (text "Welcome" *> (optional nameParser <* char '!'))
        nameParser = pack <$> (text " " *> many (noneOf "!"))
        login = LoginOrRegister <$ text "Login or Register"
        online = Online <$> (text "Online " *> natural)
        seekNew = SeekNew <$> (text "Seek new " *> natural) <*> playername <*> boardSize <*> natural <*> natural <*> optional (token player)
        boardSize = fromIntegral <$> natural
        seekRemove = SeekRemove <$> (text "Seek remove " *> natural) <*> playername <*> (fromIntegral <$> natural) <*> natural <*> natural <*> optional (token player)
        playername = pack <$> token (many (noneOf " "))
        player = (Player1 <$ char 'W') <|> (Player2 <$ char 'B')
        player' = (Player1 <$ text "white") <|> (Player2 <$ text "black")
        gameListAdd = GameListAdd <$> (text "GameList Add Game#" *> natural)
        gameListRemove = GameListRemove <$> (text "GameList Remove Game#" *> natural)
        gameStart = GameMsgStart <$> (text "Game Start " *> natural) <*> boardSize <*> playername <*> (text "vs " *> playername) <*> token player'
        gamePlace = GameMsgMove <$> (text "Game#" *> (natural <* token (text "P"))) <*> placeMove
        placeMove = g <$> token coord <*> optional (oneOf "CW")
            where
                g c Nothing = Place Flat c
                g c (Just 'C') = Place Cap c
                g c (Just 'W') = Place Standing c
        gameMove = GameMsgMove <$> (text "Game#" *> (natural <* token (text "M"))) <*> moveMove
        moveMove = (\(c1:c2:_) xs -> Move (sum (map fromInteger xs)) c1 (d c1 c2) (map fromInteger xs)) <$> some (token coord) <*> some natural
            where d c1 c2
                    | goDirection U c1 == c2 = U
                    | goDirection D c1 == c2 = D
                    | goDirection L c1 == c2 = L
                    | goDirection R c1 == c2 = R
        coord = (\f r -> (toLower f,read [r])) <$> letter <*> digit
        gameOver = GameMsgOver <$> (text "Game#" *> (natural <* token (text "Over"))) <*> gameOverParser
        gameAbandoned = GameMsgAbandoned <$> token (text "Game#" *> natural) <* text "Abandoned"
        ok = OK <$ text "OK"
        nok = NOK <$ text "NOK"

