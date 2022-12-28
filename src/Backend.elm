module Backend exposing (..)

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
i
 e e e e e

 e e e e e
I=YE=P
"""

init : ( Model, Cmd BackendMsg )
init =
    ( { undoStack = [ Baba.gridFromString initialGridStr ] }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

updateViaBaba msg model =
    let
        ( debugStr, updatedStack ) = Baba.nonGraphicsUpdate msg model.undoStack
    in
    case updatedStack of
        Just ((grid :: restOfStack) as stack) ->
            ( { model | undoStack = stack }
            , Lamdera.broadcast (GridState grid)
            )

        _ ->
            ( model, Cmd.none )

updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        Join ->

            (   model
            ,   case List.head model.undoStack of
                    Just grid ->
                        Lamdera.sendToFrontend clientId (GridState grid)

                    _ ->
                        Cmd.none
            )

        ServerMoveYou dir ->
            updateViaBaba (Baba.MoveYou dir) model

        ServerSingleKey op ->
            updateViaBaba (Baba.SingleKey op) model
