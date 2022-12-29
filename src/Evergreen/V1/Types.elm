module Evergreen.V1.Types exposing (..)

import Evergreen.V1.Baba.Baba
import Evergreen.V1.Baba.Cell
import Evergreen.V1.Baba.Graphics
import Evergreen.V1.Baba.LinkedGrid


type alias FrontendModel =
    { graphics : Evergreen.V1.Baba.Graphics.Model
    , editorContents : String
    }


type alias BackendModel =
    { undoStack : (List Evergreen.V1.Baba.Cell.Grid)
    }


type FrontendMsg
    = BabaMsg Evergreen.V1.Baba.Baba.Msg
    | GraphicsMsg Evergreen.V1.Baba.Graphics.Msg
    | BabaInput String
    | Noop


type alias SingleKeyOp = Evergreen.V1.Baba.Baba.SingleKeyOp


type ToBackend
    = Join
    | ServerMoveYou Evergreen.V1.Baba.LinkedGrid.Direction
    | ServerSingleKey SingleKeyOp
    | ServerReplaceGrid String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = GridState Evergreen.V1.Baba.Cell.Grid
    | EditorContents String