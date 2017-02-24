module Tak.Printer.PTN where

import           Tak.Types

printMoves :: [[Either GameOverState Move]] -> [String]
printMoves = zipWith printLine ([1..] :: [Int])
    where
      printLine i ms = show i ++ ". " ++ unwords (map printMove ms)

printPieceType :: PieceType -> String
printPieceType Flat = ""
printPieceType Standing = "S"
printPieceType Cap = "C"

printDirection :: Direction -> String
printDirection U = "+"
printDirection D = "-"
printDirection L = "<"
printDirection R = ">"

printMove :: Either GameOverState Move -> String
printMove (Right (Place p (f,r))) = printPieceType p ++ f:show r
printMove (Right (Move 0 (f,r) d is)) = f:show r
                              ++ printDirection d
                              ++ concatMap show is
printMove (Right (Move i x y z)) = show i ++ printMove (Right (Move 0 x y z))
printMove (Left (RoadWin Player1)) = "R-0"
printMove (Left (RoadWin Player2)) = "0-R"
printMove (Left (FlatWin Player1)) = "F-0"
printMove (Left (FlatWin Player2)) = "0-F"
printMove (Left (ResignWin Player1)) = "1-0"
printMove (Left (ResignWin Player2)) = "0-1"
printMove (Left Draw) = "1/2-1/2"
