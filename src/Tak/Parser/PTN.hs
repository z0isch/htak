module Tak.Parser.PTN where

import           Control.Applicative
import           Data.Map.Strict     (Map)
import qualified Data.Map.Strict     as M
import           Data.Maybe
import           Tak.Types
import           Text.Trifecta

type InformationalMark = String
type Comment = String
type Metadata = Map String String
type PTNMove = [Either Comment (Either GameOverState Move,Maybe InformationalMark)]
type PTNGame = (Metadata, [PTNMove])

gameParser :: Parser PTNGame
gameParser = (,) <$> metadata <*> (token whiteSpace *> some (token ptnMoveParser))
  where metadata = M.fromList <$> some (token metadataParser)

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
moveParser = try moveStoneParser <|> placeParser
  where
    moveStoneParser = Move <$> (fromMaybe 1 <$> optional int)
                           <*> coordParser
                           <*> directionParser
                           <*> ((\xs -> if null xs then [1] else xs) <$> many int <* skipOptional pieceTypeParser)
    placeParser = Place <$> pieceTypeParser
                        <*> coordParser

gameOverParser :: Parser GameOverState
gameOverParser =  choice [ Draw      <$ string "1/2-1/2"
                         , RoadWin   <$> choice [ Player1 <$ string "R-0"
                                                , Player2 <$ string "0-R"
                                                ]
                         , FlatWin   <$> choice [ Player1 <$ string "F-0"
                                                , Player2 <$ string "0-F"
                                                ]
                         , ResignWin <$> choice [ Player1 <$ string "1-0"
                                                , Player2 <$ string "0-1"
                                                ]
                         ]

informationalMarkParser :: Parser InformationalMark
informationalMarkParser = some (oneOf "?!'")

commentParser :: Parser Comment
commentParser = braces (some (noneOf "{}"))

coordParser :: Parser Coord
coordParser = (,) <$> lower <*> int

int :: Parser Int
int = read . (:[]) <$> digit

metadataParser :: Parser (String,String)
metadataParser = brackets $ (,) <$> many alphaNum <*> (space *> stringLiteral)

pieceTypeParser :: Parser PieceType
pieceTypeParser = choice [ Cap      <$ char 'C'
                         , Standing <$ char 'S'
                         , Flat     <$ skipOptional (char 'F')
                         ]

directionParser :: Parser Direction
directionParser = choice [ U <$ char '+'
                         , D <$ char '-'
                         , L <$ char '<'
                         , R <$ char '>'
                         ]
