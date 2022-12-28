module Baba.Draw exposing (init, view, Msg, Model, update)

import Html
import Html.Attributes as Attr

import Game.Resources as Resources exposing (Resources)

import Game.TwoD as Game
import Game.TwoD.Camera as Camera exposing (Camera)

import Baba.Draw.GlTest as GlTest

import Html exposing ( div, pre, text )

type Msg
  = GlCmd GlTest.Msg

type alias Model =
  { camera : Camera
  , gl : GlTest.Model
  }

spritesPng = "images/blah.png"

init : (Msg -> msg) -> ( Model, Cmd msg )
init msg =
  let
    ( glmodel, glcmd ) = GlTest.init
  in
  ( { camera = Camera.fixedWidth 8 ( 0, 0 )
    , gl = glmodel
    }
  , Cmd.map (GlCmd >> msg) glcmd
  )

view : Model -> Html.Html msg
view model = GlTest.view model.gl

update : Msg -> Model -> Model
update (GlCmd glmsg) model = 
  { model | gl = GlTest.update glmsg model.gl }
