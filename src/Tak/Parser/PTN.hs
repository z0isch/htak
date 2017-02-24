module Tak.Parser.PTN where

import           Control.Applicative
import           Data.Maybe
import           Tak.Types
import           Text.Trifecta

type InformationalMark = String
type Comment = String
type PTNMove = [Either Comment (Either GameOverState Move,Maybe InformationalMark)]

gameParser :: Parser [PTNMove]
gameParser = some (token ptnMoveParser)

ptnMoveParser :: Parser PTNMove
ptnMoveParser = lineNumParser *> some eParser
  where
    eParser = try (Left <$> token commentParser)
                <|> (Right <$> token moveAndInfo)
    lineNumParser = some digit <* (char '.' <* space)
    moveAndInfo = (,) <$> (try (Right <$> moveParser)
                          <|> (Left <$> gameOverParser))
                      <*> optional informationalMarkParser

moveParser :: Parser Move
moveParser = try moveStoneParser
             <|> placeParser
  where
    moveStoneParser = Move <$> (fromMaybe 0 <$> optional int)
                          <*> coordParser
                          <*> directionParser
                          <*> (many int <* skipOptional pieceTypeParser)
    placeParser = Place <$> pieceTypeParser
                        <*> coordParser

gameOverParser :: Parser GameOverState
gameOverParser =  drawParser
             <|> try roadWinParser
             <|> try flatWinParser
             <|> resignParser
  where
    drawParser = Draw <$ string "1/2-1/2"
    roadWinParser = try (RoadWin Player1 <$ string "R-0")
                    <|> RoadWin Player2 <$ string "0-R"
    flatWinParser = try (FlatWin Player1 <$ string "F-0")
                    <|> FlatWin Player2 <$ string "0-F"
    resignParser =  try (ResignWin Player1 <$ string "1-0")
                    <|> ResignWin Player2 <$ string "0-1"

informationalMarkParser :: Parser InformationalMark
informationalMarkParser = some (oneOf "?!'")

commentParser :: Parser Comment
commentParser = braces (some (noneOf "{}"))

coordParser :: Parser Coord
coordParser = (,) <$> lower <*> int

int :: Parser Int
int = read . (:[]) <$> digit

pieceTypeParser :: Parser PieceType
pieceTypeParser = try capParser <|> try standingParser <|> flatParser
  where
    capParser = Cap <$ char 'C'
    standingParser = Standing <$ char 'S'
    flatParser = Flat <$ skipOptional (char 'F')

directionParser :: Parser Direction
directionParser = try (U <$ char '+')
                  <|> try (D <$ char '-')
                  <|> try (L <$ char '<')
                  <|> (R <$ char '>')
