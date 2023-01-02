module Sprites exposing ( .. )

import Css
import Dict
import Html
import Html.Styled exposing ( div, img, text, toUnstyled )
import Html.Styled.Attributes exposing ( css )

import Baba.LinkedGrid exposing ( Direction(..) )
import Baba.Types as Types

floorf = floor >> toFloat

type alias Sprite =
  { url: String, imageWidth: Float
  , u: Int, v: Int
  , xOffset: Int, yOffset: Int
  , w: Int, h: Int
  --, sep: Int
  , numFrames: Int
  --, scale: Float
  }

{- 
Will need replace this with a Renderable to cover text etc, but use this everywhere first
-}

type Instance
  = SpriteInstance
    { sprite: Sprite
    , x: Float, y: Float
    , alpha: Float
    , scale: Float
    , frameNormal: Float
    }
  | TextInstance
    { text: String
    , x: Float, y: Float
    , alpha: Float
    , size: Float
    }

type alias Renderable = List Instance

renderSprite :
  Float -> Float -> Float -> Float -> Float
  -> Sprite
  -> Renderable
renderSprite x y alpha delta scale sprite =
  [ SpriteInstance
    { sprite = sprite
    , x = x, y = y
    , alpha = alpha
    , frameNormal = delta
    , scale = scale
    }
  ]

renderConnectedSprite : (Sprite -> Renderable) -> ( Int, Int ) -> (Sprite -> Renderable)
renderConnectedSprite render ( offsetU, offsetV ) =
  \sprite ->
      render
        { sprite
        | u = sprite.u - offsetU * 16
        , v = sprite.v - offsetV * 16
        }
  


renderText : 
  Float -> Float -> Float -> Float
  -> String
  -> Renderable
renderText x y alpha size text =
  [ TextInstance
    { text = text
    , x = x, y = y
    , alpha = alpha
    , size = size
    }
  ]


getDiv : Instance -> Html.Styled.Html msg
getDiv instance =
  case instance of
    SpriteInstance s ->
      div
        [ css
          [ Css.backgroundImage (Css.url s.sprite.url)
          , Css.backgroundSize (Css.px s.sprite.imageWidth)
          , Css.opacity (Css.num s.alpha)
          , Css.property "image-rendering" "pixelated"
          , Css.position Css.absolute
          , Css.property "pointer-events" "none"
          , Css.backgroundPosition2 
            (Css.px (toFloat s.sprite.u - 
              (if s.frameNormal == 1.0 then 0 else
                toFloat s.sprite.w * floorf (toFloat s.sprite.numFrames * s.frameNormal))))
            (Css.px (toFloat s.sprite.v))
          , Css.left (Css.px (s.x + toFloat s.sprite.xOffset))
          , Css.top (Css.px (s.y + toFloat s.sprite.yOffset))
          , Css.width (Css.px (toFloat s.sprite.w))
          , Css.height (Css.px (toFloat s.sprite.h))

          ]  
        ]
        []

    TextInstance t ->
      div
        [ css
          [ Css.position Css.absolute
          , Css.property "pointer-events" "none"
          , Css.left (Css.px (t.x + 3))
          , Css.top (Css.px (t.y + 14))
          , Css.fontFamily Css.sansSerif
          , Css.fontSize (Css.px 6)
          , Css.width (Css.px 16) 
          , Css.height (Css.px 12) 
          , Css.backgroundColor (Css.rgba 255 255 255 0.3)
          ] 
        ]
        [ div
          [ css
            [ Css.position Css.relative
            , Css.top (Css.px 2)
            , Css.textAlign Css.center
            ]
          ]

          [text t.text]
        ]

toHtml : Renderable -> Html.Styled.Html msg
toHtml instances =
  let
    scale = 2.0
    xoffset = scale * 160
  -- roughly 1.5 = 180 2x = 350, 2.5x = 550, 3x = 700
  in
  div 
    [ css
      [ Css.position Css.absolute
      , Css.property "transform-origin" "center"
      , Css.transform (Css.scale 2.5)
      ]
    ]
    (List.map getDiv instances)
  --|> toUnstyled



-- Baba stuff!
makeLink vOffset =
  { url = "images/sprites.png", imageWidth = 256
  , u = 0, v = -vOffset
  , xOffset = 0, yOffset = 0
  , w = 24, h = 32
  --, sep = 32
  , numFrames = 8
  --, scale = 1.0
  }

makeZelda vOffset =
  { url = "images/sprites.png", imageWidth = 256
  , u = -192, v = -vOffset
  , xOffset = 0, yOffset = 0
  , w = 24, h = 32
  --, sep = 32
  , numFrames = 2
  --, scale = 1.0
  }

makeFixed u v =
  { url = "images/sprites.png", imageWidth = 256
  , u = -u, v = -v
  , xOffset = 0, yOffset = 0
  , w = 24, h = 32
  --, sep = 32
  , numFrames = 1
  --, scale = 1.0
  }

getAnimated factory direction =
  case direction of 
    Left -> factory 0
    Right -> factory 32
    Up -> factory 64
    Down -> factory 96


animatedSprites = Dict.fromList
  [ ( 'i', getAnimated makeLink )
  , ( 'a', getAnimated makeZelda )
  ]

fixedSprites = Dict.fromList
  [ ( 'c', makeFixed 0 128 )
  , ( 'f', makeFixed 24 128 )
  , ( 'd', makeFixed 48 128 )

  , ( 'b'
    , { url = "images/sprites.png", imageWidth = 256
      , u = 108, v = 120
      , xOffset = 4, yOffset = 10
      , w = 16, h = 16
      , numFrames = 1
      }
    )

  , ( 'h', makeFixed 96 128 )
  , ( 'g', makeFixed 120 128 )
  , ( 'e'
    , { url = "images/fences.png", imageWidth = 81
      , u = 74, v = 68
      , xOffset = 4, yOffset = 10
      , w = 16, h = 16
      , numFrames = 1
      }
    )
  ]

getAnimatedSprite : Types.Noun -> Maybe (Direction -> Sprite)
getAnimatedSprite (Types.Noun c) = Dict.get c animatedSprites

getFixedSprite : Types.Noun -> Maybe Sprite
getFixedSprite (Types.Noun c) = Dict.get c fixedSprites

