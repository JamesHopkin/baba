module Evergreen.V1.Baba.Baba exposing (..)

import Evergreen.V1.Baba.Graphics
import Evergreen.V1.Baba.LinkedGrid


type SingleKeyOp
    = Undo
    | Wait
    | Ignore


type Msg
    = MoveYou Evergreen.V1.Baba.LinkedGrid.Direction
    | SingleKey SingleKeyOp
    | GraphicsMsg Evergreen.V1.Baba.Graphics.Msg