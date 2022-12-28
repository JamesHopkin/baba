module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)

import Baba.Baba as Baba
import Baba.Cell as Cell
import Baba.Graphics as Graphics
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias FrontendModel =
    { graphics : Graphics.Model
    }


type alias BackendModel =
    { undoStack : List Cell.Grid
    }


type alias SingleKeyOp = Baba.SingleKeyOp

type FrontendMsg
    = MoveYou Direction
    | SingleKey SingleKeyOp
    | GraphicsMsg Graphics.Msg
    -- just latest grid for now
    --| FromServer Cell.Grid
    | Noop


type ToBackend
    = Join
    | ServerMoveYou Direction
    | ServerSingleKey SingleKeyOp


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = GridState Cell.Grid