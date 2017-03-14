module Main where

import           Control.Lens
import qualified Data.ByteString              as B
import qualified Data.ByteString.Lazy         as BL
import           Data.Either
import           Data.List
import qualified Data.Map.Strict              as M
import           Data.Maybe
import           Network.Wreq
import           Tak.Parser.PTN
import           Tak.Printer.PTN
import           Tak.Types
import           Text.PrettyPrint.ANSI.Leijen (displayS, renderPretty)
import           Text.Trifecta

main :: IO ()
main = print (scanl' makeMove (initialGameState 6) mvs)
  where
    mvs = case parseString gameParser mempty game1 of
      Failure _ -> []
      Success (_,ms) -> concatMap (rights . map fst . rights) ms

getPlayTakPTNString :: Int -> IO B.ByteString
getPlayTakPTNString i = do
  r <- get $ "https://playtak.com/games/" ++ show i
  return (BL.toStrict $ r^.responseBody)

getGameFromPlayTak :: Int -> IO PTNGame
getGameFromPlayTak i = do
  g <- getPlayTakPTNString i
  return $ case parseByteString gameParser mempty g of
        Failure xs -> error $ displayS (renderPretty 0.8 80 xs) ""
        Success s -> s

getMoves :: PTNGame -> [Move]
getMoves (_,ptnMs) = concatMap (rights . map fst .rights) ptnMs

getGameStates :: PTNGame -> [GameState]
getGameStates g = scanl' makeMove (initialGameState (getBoardSize g)) $ getMoves g

getBoardSize :: PTNGame -> BoardSize
getBoardSize (md,_) = read $ md M.! "Size"

validateGameState :: PTNGame -> Bool
validateGameState g@(md,_)
  | resignWin = movesChecker
  | otherwise = movesChecker && winnerChecker
  where
    resignWin = gameWinner == "1-0" || gameWinner == "0-1"
    movesChecker = and $ zipWith (\m gs -> m `elem` moves gs) ms gss
    winnerChecker = fromMaybe False w
    w = (== gameWinner) . printMove . Left  <$> last gss^.gsGameOverState
    gameWinner = md M.! "Result"
    ms = getMoves g
    gss = getGameStates g

printBoard :: GameState -> IO ()
printBoard gs = mapM_ print $ M.toList (gs^.gsBoard)

game1 :: String
game1 = "1. a6 a1\n2. c3 b2\n3. d3 b3\n4. b4 Cc4\n5. a3 b5\n6. a2 b1\n7. Cc2 b6\n8. 1c2<1 d2\n9. a4 d4\n10. c5 d5\n11. 1c5<1 1c4<1\n12. e3 f3\n13. e2 1b3>1\n14. c2 d1\n15. 1d3-1 1d1+1\n16. 1e2<1 Se2\n17. a5 1e2<1\n18. 2b2>11 2b4+2\n19. 1a5+1 a5\n20. 4d2<22 Se2\n21. 3c2+3 1e2<1\n22. e2 f4\n23. e4 c5\n24. d3 2d2+2\n25. 1a4+1 3b5<3\n26. 5c3+14 5a5>14\n27. a4 6c5<15\n28. c6 e5\n29. 1c4+1 1b5>1\n30. 1c6-1 1b5>1\n31. b4 6c5-1122"

tournamentGames :: [Int]
tournamentGames = [94008,92732,92738,94210,94219,93524,93532,93901,93915,93874,93878,93981,93982,92541,92543,93037,93060,93864,93871,93971,93977,94343,94344,93959,93962,93512,93513,93988,93997,93340,93350,93613,93619,94335,94336,92296,92297,92859,92862,92873,92891,92690,92699,91953,91962,92361,92385,94239,94246,94038,94042,92400,92404,92383,92392,92657,92661,93899,93903,93631,93632,93554,93555,93538,93539,95173,95182,96328,96340,94948,94953,96274,96280,96636,96638,95264,95268,95709,95710,95922,95930,95550,95555,94982,94985,95274,95276,95423,95425,96437,96443,96219,96222,96193,96194,96984,96990,94883,94884,96463,96468,95303,95305,96806,96809,94930,94934,95670,95682,96765,96785,96787,96798,94980,94981,96354,96365,95342,95344,96820,96831,96048,96049,96033,96034,96665,96670,96094,96095,96404,96408,96325,96333,96869,96870,96439,96446,95722,95726,97143,97146,95692,95694,96405,96407,96567,96569,96459,96461,96036,96037,96755,96758,98924,98935,97471,97494,99653,99655,98819,98820,99000,99015,98923,98930,98436,98437,98574,98576,98896,98906,98194,98198,98786,98787,98974,98981,98204,98210,99299,99300,98752,98753,99610,99613,99207,99209,99034,99037,98663,98668,98294,98295,98746,98754,99193,99198,98126,98128,99656,99657,98136,98148,98390,98399,98996,98999,98883,98934,98944,99064,99065,99671,99673,98395,98400,98442,98444,99231,99237,98382,98386,99265,99271,98193,98199,99035,99036,99609,99612,99364,99366,98398,98401,101008,101009,101354,101355,100711,100718,100023,100024,101628,101635,101199,101200,100491,100493,101580,101581,100649,100653,100477,100483,100171,100188,99922,99923,101502,101511,100494,100495,101495,101499,100476,100481,100201,100209,100489,100490,101114,101116,101517,101534,101016,101019,101030,101034,101387,101389,100210,100213,101341,101347,101310,101311,100168,100177,100733,100734,100982,100995,100626,100627,101054,101066,101097,101102,101276,101278,101312,101317,100645,100652,100945,100947,101029,101033,101392,101394,101599,101600,101086,101088,100791,100792,101338,101349,101880,101887,103109,103118,103151,103146,103451,103466,103555,103558,103001,103010,102581,102582,102559,102565,102893,102912,103469,103477,103002,103005,103399,103404,103368,103369,103909,103910,102555,102557,101933,101973,103370,103372,102751,102755,102795,102802,103923,103924,102853,102856,103085,103088,103707,103708,102566,102575,103148,103150,103473,103468,102671,102673,103074,103066,102778,102785,102669,102675,102517,102520,103039,103040,103323,103327,102846,102847,102848,102849,102809,102813,103354,103357,104968,104970,105797,105804,105320,105323,105805,105817,105529,105533,105485,105487,104993,105002,104716,104718,105586,105587,105784,105789,105450,105453,105358,105367,104939,104942,105895,105896,105926,105930,104893,104902,106115,106117,105010,105011,105801,105807,105802,105803,105866,105879,104961,104962,105861,105864,104997,105000,105018,105024,105785,105794,105504,105505,105031,105033,104675,104678,105443,105444,105696,105697,105437,105439,104757,104759,106827,106830,107431,107432,107223,107225,108333,108340,107465,107478,107977,107982,107439,107442,108270,108278,107212,107215,107510,107523,106654,106655,107609,107610,107216,107219,107574,107581,106817,106818,107794,107800,106540,106544,107861,107864,107480,107483,106725,106726,108020,108027,107088,107092,107875,107878,107014,107020,106924,106927,107949,107956,107207,107203,108312,108313,107268,107269,107827,107832,110170,110173,110513,110545,110778,110793,110681,110699,110151,110156,110166,110171,110195,110202,111111,111126,112313,112322,110231,110236,110484,110493,110807,110827,110159,110165,110289,110292,110505,110509,110675,110678,112061,112095,113053,113085,113496,113515,113104,113127,113529,113530,113160,113180,113698,113711,113486,113487,115508,115521,115589,115593,114549,114571,115224,115237,120338,120346,117941,117959,123113,123120,113508,113514,113542,113543,113411,113432,113001,113009,113519,113532,112190,112194,113101,113114,115728,115731,114880,114900,115865,115871,115566,115575,114199,114216,115187,115197,114273,114288,117122,117123,117566,117569,117881,117884,116404,116418,118296,118337,120344,120351,120105,120125,118386,118392,122989,122996,122222,122232,126523,126551,123240,123247,127069,127082,127404,127414,128346,128351]
