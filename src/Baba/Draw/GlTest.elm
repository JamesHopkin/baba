module Baba.Draw.GlTest exposing (view, Model, Msg, init, update)
import Math.Vector2 exposing (Vec2, vec2)
import Math.Vector3 exposing (Vec3, vec3)
import Math.Matrix4 exposing (Mat4)

import WebGL
import WebGL.Settings.Blend as Blend
import WebGL.Settings.DepthTest as DepthTest
import WebGL.Texture exposing (Texture)

import Html
import Html.Attributes as Attr
import Task


-- remove when relevant stuff moved to Graphics
import Baba.Cell as Cell


spritesPng = "images/blah2.png"

type Msg
    = LoadedTexture String (Result WebGL.Texture.Error Texture)

type alias Model = Maybe Texture
init : ( Model, Cmd Msg )
init =
  let
    defOpts = WebGL.Texture.defaultOptions
  in
  ( Nothing
  , Task.attempt
      ( LoadedTexture spritesPng )
      ( WebGL.Texture.loadWith
        { defOpts
        | magnify = WebGL.Texture.nearest

        } spritesPng )
  )

update : Msg -> Model -> Model
update (LoadedTexture _ result) _ =

  case result of
    Ok tex ->
      Just tex

    Err err ->
      let
        dummy = Debug.log "texture error!" [err]
      in
      Nothing

{-===========
|           |
|  SHADERS  |
|           |
===========-}

-- single colour
vertColoredShape : WebGL.Shader
                    Vertex
                    { a | transform : Mat4, cameraProj : Mat4 }
                    { vcoord : Vec2 }
vertColoredShape =
    [glsl|
attribute vec2 position;
uniform mat4 transform;
uniform mat4 cameraProj;
varying vec2 vcoord;
void main() {
    gl_Position = cameraProj*transform*vec4(position, 0, 1);
    vcoord = position.xy;
}
|]

fragUniColor : WebGL.Shader 
                {}
                { u | color : Vec3 }
                { vcoord : Vec2 }
fragUniColor =
    [glsl|
precision mediump float;
uniform vec3 color;
varying vec2 vcoord;
void main() {
    gl_FragColor = vec4(color, 1);
}
|]

{-| textured -}
fragTextured : WebGL.Shader
                {}
                { u | texture : Texture, tl : Vec2, wh : Vec2 }
                { vcoord : Vec2 }
fragTextured =
    [glsl|
precision mediump float;
uniform sampler2D texture;
uniform vec2 tl;
uniform vec2 wh;
varying vec2 vcoord;
void main () {
    gl_FragColor = texture2D(texture, (vcoord * wh) + tl);
}
|]


{-===========
|           |
|  SHAPES   |
|           |
===========-}

type alias Vertex = { position : Vec2 }

unitSquare : WebGL.Mesh Vertex
unitSquare =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 0 1)
          , Vertex (vec2 1 0)
          )
        , ( Vertex (vec2 0 1)
          , Vertex (vec2 1 0)
          , Vertex (vec2 1 1)
          )
        ]

square : Float -> Float -> WebGL.Mesh Vertex
square w h =
    WebGL.triangles
        [ ( Vertex (vec2 0 0)
          , Vertex (vec2 0 h)
          , Vertex (vec2 w 0)
          )
        , ( Vertex (vec2 0 h)
          , Vertex (vec2 w 0)
          , Vertex (vec2 w h)
          )
        ]

{-===========
|           |
|  Camera   |
|           |
===========-}

scale a ( x, y ) =
    ( a * x, a * y )

{-
  try to understand: getViewSize takes a size and a "camera", which is itself a generic size

e.g.
  x, y = 10, 0
  cam width = 30
  size = ( 250, 200 )

  view size = ( 30, 30 * (200/250) )
  w, h = ( 15, bit less than 15 )
-}

cameraView : ( Float, Float ) -> Float -> ( Float, Float ) -> Mat4
cameraView ( x, y ) cameraWidth viewSizePixels =
    let
        ( w, h ) =
            getViewSize viewSizePixels cameraWidth
    in
    Math.Matrix4.makeOrtho2D x (x + w) y (y + h)


{- converts wh in pixels to game units -}
getViewSize : ( Float, Float ) -> Float -> ( Float, Float )
getViewSize ( w, h ) cameraWidth = ( cameraWidth, cameraWidth * h / w )

{-===========
|           |
|  Render   |
|           |
===========-}

renderTransparent :
  WebGL.Shader attributes uniforms varyings ->  -- vertex shader
  WebGL.Shader {} uniforms varyings ->          -- fragment shader
  WebGL.Mesh attributes ->
  uniforms ->
  WebGL.Entity
renderTransparent =
    WebGL.entityWith
        [ Blend.custom
            { r = 0, g = 0, b = 0, a = 0
            , color = Blend.customAdd Blend.srcAlpha Blend.oneMinusSrcAlpha
            , alpha = Blend.customAdd Blend.one Blend.oneMinusSrcAlpha
            }
        , DepthTest.always { write = True, near = 0, far = 1.0 }
        ]

makeTransform : ( Float, Float ) -> ( Float, Float ) -> Mat4
makeTransform ( x, y ) ( w, h ) = 
  Math.Matrix4.mul
    (Math.Matrix4.makeTranslate (vec3 x y 0))
    (Math.Matrix4.makeScale (vec3 w h 1))

            --M4.makeScale (vec3 w h 1)

{-

Screen width in game units is 20


-}

screenDims = ( 100, 300 )
testCamera = cameraView ( 0, 0 ) 4 screenDims -- origin, 3 grid squares across (plus half width border)

exampleRect =
  renderTransparent vertColoredShape fragUniColor unitSquare
    { transform = makeTransform ( 0, 3.0 ) ( 1.0, 1.0 )
    , color = vec3 1.0 0.5 0.0 {- rgb -}
                              {- pos -} {- view width    -}
                                        {- in game units -}
    , cameraProj = testCamera
    }

type alias ImageRecord = { tl : ( Int, Int )
  , wh : ( Int, Int )
  , offset : ( Int, Int )
  }

type Drawable =
  Image ImageRecord

link = Image { tl = ( 0, 0 ), wh = ( 24, 32 ), offset = ( 0, 2 ) }

rock = Image { tl = ( 4, 138 ), wh = ( 16, 16 ), offset = ( 0, 0 ) }
key = Image { tl = ( 28, 138 ), wh = ( 16, 16 ), offset = ( 0, 0 ) }


{-

  e.g. height 16, want y to go from 12->16, so .5 maps to 14/16

  t = 12/16
  h = 4/16


seems like 4. 

-}

gridSize = 16.0


exampleTexturedRect camera texture pos =
  let
    ( tw, th ) = WebGL.Texture.size texture

    spriteWidth = 24.0
    spriteHeight = 24.0

    t = 1.0 - (138.0 + spriteHeight) / toFloat th
  in
  renderTransparent vertColoredShape fragTextured (square spriteWidth spriteHeight)
    { transform = makeTransform pos ( 1.0 / gridSize, 1.0 / gridSize )
    , cameraProj = camera
    , tl = vec2 (4 / toFloat tw) t
    , wh = vec2 (1 / toFloat tw) (1 / toFloat th)
    , texture = texture
    }

tupToFloat ( x, y ) = ( toFloat x, toFloat y )

drawImage : Mat4 -> Texture -> ImageRecord -> ( Float, Float ) -> WebGL.Entity
drawImage camera texture image pos = 
  let
    ( tw, th ) = WebGL.Texture.size texture

    ( spriteLeft, spriteTop ) = tupToFloat image.tl
    ( sw, sh ) = tupToFloat image.wh

    ( x, y ) = pos
    -- centralise and offset (based on image requirement and for border)
    computedPos =
      ( x + 0.5 + ((gridSize - sw) / 2.0 + toFloat (Tuple.first image.offset)) / gridSize
      , y + 0.5 + ((gridSize - sh) / 2.0 + toFloat (Tuple.second image.offset)) / gridSize
      )

    t = 1.0 - (spriteTop + sh) / toFloat th
  in
    renderTransparent vertColoredShape fragTextured (square sw sh)
      { cameraProj = camera
      , texture = texture
      , tl = vec2 (spriteLeft / toFloat tw) (1.0 - (spriteTop + sh) / toFloat th)
      , wh = vec2 (1 / toFloat tw) (1 / toFloat th)
      , transform = makeTransform computedPos ( 1.0 / gridSize, 1.0 / gridSize )
      }

view : Model -> Html.Html msg
view model =
      let
        ( w, h ) = screenDims

        textured =
          case model of
            Just texture ->
              let
                draw (Image image) = drawImage testCamera texture image
              in
                [ draw rock ( 0, 1 )
                , draw key ( 1, 1 )
                , draw link ( 0, 0 )
                ]

            _ ->
              []

      in
      WebGL.toHtmlWith
        [ WebGL.alpha False, WebGL.depth 1, WebGL.antialias ]
        [ Attr.width w, Attr.height h ] -- probably put the screen dims in the model
        ([ exampleRect ] ++ textured)



{- 
  Stuff to go replace Graphics.elm

  two phases: grid to object/pos list, objects to GL

  how to make Image more generic?
-}


render : Maybe Cell.Grid -> Html.Html msg
render grid = Html.div [] [ Html.text "no grid" ]


-- most readable way to specify what to draw in graphics code?
    -- e.g. two aspects to animated: run animated code, which frames?

{- important: do I pass frame delta down or return "animated sprite"

  needs to be on the Baba side: very specific that there's a single delta for
  most stuff animating/moving

  so pass over BabaAnimated, can use animated sprite function at that point,
    maybe work out well if t is last argument of that function

for simplicitly, maybe do composite sprites on the Baba side, see what utils
can add on Draw side
-}

bind : ( number, a ) -> (a -> ( number, b )) -> ( number, b )
bind ( w, a ) f =
  let
    ( wp, b ) = f a
    nlog = Debug.log "number" [w, wp]
    alog = Debug.log "a" a
    blog = Debug.log "b" b
  in
  ( w + wp, b )

return v = ( 0, v )

sequence : List ( Int, a ) -> ( Int, List a )
sequence l = 
  case l of
    [] ->
      return []
    x :: xs ->
      bind x (\xp -> 
        bind (sequence xs) (\xsp ->
          return ( xp :: xsp )
      ))


tell n = ( n, 0 )

