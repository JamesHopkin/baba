module Evergreen.V1.Baba.Graphics exposing (..)

import Evergreen.V1.Baba.Cell
import Time


type alias Model = 
    { grid : (Maybe Evergreen.V1.Baba.Cell.Grid)
    , previousGrid : (Maybe Evergreen.V1.Baba.Cell.Grid)
    , lastAnimTime : Time.Posix
    , lastUpdateTime : Time.Posix
    }


type Msg
    = AnimationFrame Time.Posix