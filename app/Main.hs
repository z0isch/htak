module Main where

import           Control.Lens
import           Data.Either
import           Data.List
import qualified Data.Map.Strict as M
import           Tak.Parser.PTN
import           Tak.Types
import           Text.Trifecta

main :: IO ()
main = print (scanl' makeMove (initialGameState 6) mvs)
  where
    mvs = case parseString gameParser mempty game1 of
      Failure _ -> []
      Success ms -> concatMap rights $ map (map fst.rights) ms

movesChecker :: IO [Bool]
movesChecker = do
  mvs <- concatMap rights <$> getMoves "./ptn/game1.ptn"
  let gss = scanl' makeMove (initialGameState 6) mvs
  return $ zipWith (\m gs -> m `elem` moves gs) mvs gss

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

game1 :: String
game1 = "1. a6 a1\n2. c3 b2\n3. d3 b3\n4. b4 Cc4\n5. a3 b5\n6. a2 b1\n7. Cc2 b6\n8. 1c2<1 d2\n9. a4 d4\n10. c5 d5\n11. 1c5<1 1c4<1\n12. e3 f3\n13. e2 1b3>1\n14. c2 d1\n15. 1d3-1 1d1+1\n16. 1e2<1 Se2\n17. a5 1e2<1\n18. 2b2>11 2b4+2\n19. 1a5+1 a5\n20. 4d2<22 Se2\n21. 3c2+3 1e2<1\n22. e2 f4\n23. e4 c5\n24. d3 2d2+2\n25. 1a4+1 3b5<3\n26. 5c3+14 5a5>14\n27. a4 6c5<15\n28. c6 e5\n29. 1c4+1 1b5>1\n30. 1c6-1 1b5>1\n31. b4 6c5-1122"