module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Url exposing (Url)

import Baba.Baba as Baba
import Baba.Cell as Cell
import Baba.Graphics as Graphics
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias GameId = String

type alias FrontendModel =
    { graphics : Graphics.Model
    , editorContents : String
    , gameId : GameId
    }


type alias BackendModel =
    { games : Dict GameId (List Cell.Grid)
    }


type alias SingleKeyOp = Baba.SingleKeyOp

type FrontendMsg
    = BabaMsg Baba.Msg
    | GraphicsMsg Graphics.Msg
    | BabaInput String

    -- just latest grid for now
    --| FromServer Cell.Grid
    | Noop


type ToBackend
    = Join GameId
    | ServerMoveYou GameId Direction
    | ServerSingleKey GameId SingleKeyOp
    | ServerReplaceGrid GameId String


type BackendMsg
    = NoOpBackendMsg

{- would ideally map client ids to game ids for validation and
    specific broadcasts
  for now, make a broadcast function taking game id but ignore it
-}

-- can remove gameId if I make broadcast specific to the game

type ToFrontend
    = GridState GameId Cell.Grid
    | EditorContents GameId String
