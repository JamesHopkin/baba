module Evergreen.V1.Baba.LinkedGrid exposing (..)

import Array


type Direction
    = Up
    | Right
    | Down
    | Left


type LinkedGrid el
    = LinkedGrid el Int Int (Array.Array (Array.Array el))