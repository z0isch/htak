{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Tak.PlayTak.Printer where

import           Data.Monoid
import           Data.String.Interpolate
import           Data.Text               (Text, pack, strip, toUpper)
import qualified Data.Text               as T
import           Tak.PlayTak.Types
import           Tak.Types

showCommand :: PlayTakCommand -> Text
showCommand LoginGuest = "Login Guest"
showCommand Ping = "PING"
showCommand (Seek size clock interval mp) = strip $ pack $
    [i|Seek #{size} #{clock} #{interval} #{maybe "" playerShow mp}|]
showCommand (GameCmdMove gameNumber (Place pT coord)) = strip $ pack $
    [i|Game\##{gameNumber} P #{coordShow coord} #{pieceTypeShow pT}|]
showCommand (GameCmdMove gameNumber (Move _ coord dir xs)) = strip $ pack $
    [i|Game\##{gameNumber} M #{sqs} #{pcs}|]
    where
        sqs = T.unwords $ map coordShow
            $ foldl (\cs _ -> cs ++ [goDirection dir (last cs)]) [coord] xs
        pcs = T.unwords $ map (pack . show) xs
showCommand _ = ""

coordShow :: Coord -> Text
coordShow (file,rank) = toUpper (pack [file]) <> pack (show rank)

pieceTypeShow :: PieceType -> Text
pieceTypeShow Flat = ""
pieceTypeShow Standing = "W"
pieceTypeShow Cap = "C"

playerShow :: Player -> Text
playerShow Player1 = "W"
playerShow Player2 = "B"
