module Frontend exposing (..)

import Browser
import Browser.Events
import Browser.Navigation as Nav
import Css
import Css.Global
import Html
import Html.Styled.Attributes as Attr exposing ( css, cols, rows,width, height )
import Html.Styled.Events exposing ( onInput )
import Html.Styled exposing ( div, pre, text, textarea, toUnstyled )
import Json.Decode as Decode
import Lamdera
import Types exposing (..)
import Url

import Baba.Baba as Baba
import Baba.Graphics as Graphics
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )


{-
ln -s ~/code/from\ dropbox/elm/src/Baba ~/code/baba/src/Baba
-}

type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlChange = \_ -> Types.Noop
        , onUrlRequest = \_ -> Types.Noop
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \_ -> subscription
        , view = view
        }

{-
Working out how Bezique initial message to backend worked:

    non-admin assumed Bezique (can't remember how I chose to run 2048, maybe never hooked it up)
        Bezique.frontendInit
            <|  Types.BeziqueToBackend >> Types.GameToBackend >> Lamdera.sendToBackend
                        |                               |
            BeziqueToBackend Bezique.ToBackendMsg       |
                                                        |
                                        GameToBackend GameToBackendMsg

? only Bezique to-backend message seems to be PlayCard, so how does init work?
    - to-backend is either request game or game message (only )

-}



init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    let
        gameId = url.path
    in
    ( { graphics = Graphics.init
      , editorContents = Baba.initialGridStr
      , gameId = gameId
      }
    , Lamdera.sendToBackend (Join gameId)
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        BabaMsg (Baba.MoveYou dir) ->
            ( model, Lamdera.sendToBackend (ServerMoveYou model.gameId dir) )

        BabaMsg (Baba.SingleKey op) ->
            ( model, Lamdera.sendToBackend (ServerSingleKey model.gameId op) )

        BabaMsg _ ->
            ( model, Cmd.none )

        BabaInput gridStr ->
            ( model, Lamdera.sendToBackend (ServerReplaceGrid model.gameId gridStr) )

        GraphicsMsg graphicsMsg ->
            ( { model | graphics = Graphics.update graphicsMsg model.graphics }
            , Cmd.none
            )

        Noop ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    ( case msg of
        GridState gameId grid ->
            if gameId == model.gameId then
                { model
                | graphics = Graphics.setGrid grid model.graphics
                }
            else
                model

        EditorContents gameId gridStr ->
            if gameId == model.gameId then
                { model
                | editorContents = gridStr
                }
            else
                model

    , Cmd.none
    )
            



view : Model -> Browser.Document FrontendMsg
view model =
    { title = ""
    , body =
        [   div []
                [   Css.Global.global
                    [   Css.Global.body
                        (--css
                            [   Css.backgroundColor (Css.rgb 74 156 74)
                            ]
                        )
                    ]
                ,   Graphics.view GraphicsMsg model.graphics
                ,   div
                    [   css [ Css.float Css.right, Css.fontFamily Css.monospace ]
                    ]
                    [ textarea
                        [ rows 20, cols 20, onInput BabaInput ]
                        []
                    , pre
                        [ height 20, width 20 ]
                        [ text model.editorContents ]
                    ]
                ]
            |> toUnstyled
        ] 
    }




subscription : Sub FrontendMsg
subscription = 
    Sub.batch
        [ Browser.Events.onKeyDown (Baba.keyDecoder BabaMsg)
        , Graphics.subscription GraphicsMsg
        ]
