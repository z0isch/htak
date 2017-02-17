module Tak.Printer.PTN where

import           Tak.Types

printMoves :: [[Move]] -> [String]
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

printMove :: Move -> String
printMove (Place p (f,r)) = printPieceType p ++ f:show r
printMove (Move 0 (f,r) d is) = f:show r
                              ++ printDirection d
                              ++ concatMap show is
printMove (Move i x y z) = show i ++ printMove (Move 0 x y z)
printMove (RoadWin Player1) = "R-0"
printMove (RoadWin Player2) = "0-R"
printMove (FlatWin Player1) = "F-0"
printMove (FlatWin Player2) = "0-F"
printMove (ResignWin Player1) = "1-0"
printMove (ResignWin Player2) = "0-1"
printMove Draw = "1/2-1/2"
