module Main where

import           Control.Lens
import           Data.Either
import           Data.List
import qualified Data.Map.Strict as M
import           Tak.Parser.PTN
import           Tak.Types
import           Text.Trifecta

main :: IO ()
main =  print 1

getGame :: IO [GameState]
getGame =  do
  mvs <- concatMap rights <$> getMoves "./ptn/game1.ptn"
  return $ scanl' makeMove (initialGameState 6) mvs

printBoard :: GameState -> IO ()
printBoard gs = mapM_ print $ M.toList (gs^.gsBoard)

getMoves :: String -> IO [[Either GameOverState Move]]
getMoves f = do
  result <- parseFromFile gameParser f
  case result of
    Nothing -> return []
    Just mvs -> return $ map (map fst . rights) mvs
