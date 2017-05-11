module Tak.PlayTak.Types where

import           Data.Text
import           Tak.Types

type GameNumber = Text
type PlayerName = Text
type SeekNumber = Text
type Room = Text

data PlayTakMessage = Welcome (Maybe PlayerName)
                     | LoginOrRegister
                     | GameListAdd
                     | GameListRemove
                     | GameMsgStart GameNumber PlayerName PlayerName Player
                     | GameMsgMove GameNumber Move
                     | GameMsgTime GameNumber Integer Integer
                     | GameMsgOver GameNumber GameOverState
                     | GameMsgOfferDraw GameNumber
                     | GameMsgRemoveDraw GameNumber
                     | GameMsgRequestUndo GameNumber
                     | GameMsgRemoveUndo GameNumber
                     | GameMsgUndo GameNumber
                     | GameMsgAbandoned GameNumber
                     | SeekNew SeekNumber PlayerName BoardSize Integer (Maybe Player)
                     | SeekRemove SeekNumber PlayerName BoardSize Integer (Maybe Player)
                     | Observe
                     | Shout PlayerName Text
                     | Joined Room
                     | Left Room
                     | ShoutRoom Room PlayerName Text
                     | Tell PlayerName Text
                     | Told PlayerName Text
                     | Message Text
                     | Error Text
                     | Online Integer
                     | OK
                     | NOK
    deriving (Eq, Show)

data PlayTakCommand = Client Text
                    | Register Text Text
                    | Login Text Text
                    | LoginGuest
                    | Seek BoardSize Integer Integer (Maybe Player)
                    | SeekAccept SeekNumber
                    | GameCmdMove GameNumber Move
                    | GameCmdOfferDraw GameNumber
                    | GameCmdRemoveDraw GameNumber
                    | GameCmdResign GameNumber
                    | GameCmdShow GameNumber
                    | GameCmdRequestUndo GameNumber
                    | GameCmdRemoveUndo GameNumber
                    | SeekList
                    | GameList
                    | CmdObserve GameNumber
                    | GameCmdShowSq GameNumber Coord
                    | CmdShout Text
                    | JoinRoom Room
                    | CmdShoutRoom Room Text
                    | LeaveRoom Room
                    | CmdTell PlayerName Text
                    | Ping
                    | Quit
    deriving (Eq, Show)
