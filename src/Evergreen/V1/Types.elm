module Evergreen.V1.Types exposing (..)

import Evergreen.V1.Baba.Baba
import Evergreen.V1.Baba.Cell
import Evergreen.V1.Baba.Graphics
import Evergreen.V1.Baba.LinkedGrid


type alias FrontendModel =
    { graphics : Evergreen.V1.Baba.Graphics.Model
    }


type alias BackendModel =
    { undoStack : (List Evergreen.V1.Baba.Cell.Grid)
    }


type alias SingleKeyOp = Evergreen.V1.Baba.Baba.SingleKeyOp


type FrontendMsg
    = MoveYou Evergreen.V1.Baba.LinkedGrid.Direction
    | SingleKey SingleKeyOp
    | GraphicsMsg Evergreen.V1.Baba.Graphics.Msg
    | Noop


type ToBackend
    = Join
    | ServerMoveYou Evergreen.V1.Baba.LinkedGrid.Direction
    | ServerSingleKey SingleKeyOp


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = GridState Evergreen.V1.Baba.Cell.Grid