module Backend exposing (..)

import Dict
import Html
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)

import Baba.Baba as Baba


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


initialGridStr = """
 A eeeeeeee
 = e      e
 S e i  c e
   e   C  e
B=Ke E= c e
   e _    e
eeee X &  eeee
e    AI d    e
e  M  =  C=P e
ebba DY    L e
ebbb   e F=T e
efbb   e     e
eeeeeeeeeeeeee
"""

fallbackGrid = Baba.gridFromString "i I=Y"

init : ( Model, Cmd BackendMsg )
init =
    ( { games = Dict.empty }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

updateViaBaba : GameId -> Baba.Msg -> BackendModel -> ( BackendModel, Cmd BackendMsg )
updateViaBaba gameId msg model =
    case Dict.get gameId model.games of
        Just undoStack ->
            let
                ( debugStr, updatedStack ) = Baba.nonGraphicsUpdate msg undoStack
            in
            case updatedStack of
                Just ((grid :: restOfStack) as stack) ->
                    ( { model | games = Dict.insert gameId stack model.games }
                    , broadcast gameId (GridState gameId grid)
                    )

                _ ->
                    ( model, Cmd.none )

        _ ->
            ( model, Cmd.none )

broadcast : GameId -> ToFrontend -> Cmd BackendMsg
broadcast _ msg = Lamdera.broadcast msg


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Join gameId ->
            case Dict.get gameId model.games of
                Just undoStack ->
                    (   model
                    ,   case List.head undoStack of
                            Just grid ->
                                Lamdera.sendToFrontend clientId (GridState gameId grid)

                            _ ->
                                Cmd.none
                    )

                Nothing ->
                    let
                        newGrid = Baba.gridFromString initialGridStr
                    in
                    (   {   model
                        |   games = Dict.insert gameId [ newGrid ] model.games
                        }
                    ,   Lamdera.sendToFrontend clientId (GridState gameId newGrid)
                    )

        ServerMoveYou gameId dir ->
            updateViaBaba gameId (Baba.MoveYou dir) model

        ServerSingleKey gameId op ->
            updateViaBaba gameId (Baba.SingleKey op) model

        ServerReplaceGrid gameId gridStr ->
            case Dict.get gameId model.games of
                Just undoStack ->
                    let
                        grid = Baba.gridFromString gridStr
                    in
                    ( { model | games = Dict.insert gameId [ grid ] model.games }
                      , Cmd.batch
                        [ broadcast gameId (GridState gameId grid)
                        , broadcast gameId (EditorContents gameId gridStr)
                        ]
                    )

                Nothing ->
                    ( model, Cmd.none )
