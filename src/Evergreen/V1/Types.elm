module Evergreen.V1.Types exposing (..)

import Dict
import Evergreen.V1.Baba.Baba
import Evergreen.V1.Baba.Cell
import Evergreen.V1.Baba.Graphics
import Evergreen.V1.Baba.LinkedGrid


type alias GameId = String


type alias FrontendModel =
    { graphics : Evergreen.V1.Baba.Graphics.Model
    , editorContents : String
    , gameId : GameId
    }


type alias BackendModel =
    { games : (Dict.Dict GameId (List Evergreen.V1.Baba.Cell.Grid))
    }


type FrontendMsg
    = BabaMsg Evergreen.V1.Baba.Baba.Msg
    | GraphicsMsg Evergreen.V1.Baba.Graphics.Msg
    | BabaInput String
    | Noop


type alias SingleKeyOp = Evergreen.V1.Baba.Baba.SingleKeyOp


type ToBackend
    = Join GameId
    | ServerMoveYou GameId Evergreen.V1.Baba.LinkedGrid.Direction
    | ServerSingleKey GameId SingleKeyOp
    | ServerReplaceGrid GameId String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = GridState GameId Evergreen.V1.Baba.Cell.Grid
    | EditorContents GameId String