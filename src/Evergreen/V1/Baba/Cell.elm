module Evergreen.V1.Baba.Cell exposing (..)

import Evergreen.V1.Baba.LinkedGrid
import Evergreen.V1.Baba.Types


type ObjectKind
    = Instance Evergreen.V1.Baba.Types.Noun
    | Text Evergreen.V1.Baba.Types.Text


type alias ObjectState = 
    { word : ObjectKind
    , direction : Evergreen.V1.Baba.LinkedGrid.Direction
    , flags : Int
    , lastMovedTick : Int
    }


type Object
    = Object Int ObjectState


type alias Cell = 
    { contents : (List Object)
    , viewHints : (Maybe Evergreen.V1.Baba.Types.RuleHint)
    }


type alias Grid = (Evergreen.V1.Baba.LinkedGrid.LinkedGrid Cell)