module Main where

import           Data.Either
import           Tak.Parser.PTN
import           Tak.Printer.PTN
import           Tak.Types
import           Text.Trifecta

main :: IO ()
main =  do
  mvs <- getMoves "./ptn/game1.ptn"
  mapM_ putStrLn (printMoves mvs)

getMoves :: String -> IO [[Move]]
getMoves f = do
  result <- parseFromFile gameParser f
  case result of
    Nothing -> return []
    Just mvs -> return $ map (map fst . rights) mvs
