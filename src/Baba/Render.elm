module Baba.Render

import Basics exposing ( toFloat, floor )

cellDimensionInt = 16
cellDimension = toFloat cellDimensionInt

gt = transform [scale 2.0 2.0, translate cellDimension (cellDimension/2.0)]

{-
 want to say: render relative to grid

 5, 5: means TL a centre of (5, 5) grid square
 have centred function, that uses above to centre sprite in/around (5, 5)


 too confusing having each function produce actual renderable
 make list of things to draw

 latest idea: individual draw functions return offset and scale
 grid draw function adds in grid relative position
-}

type Sheet = Sprites | Words

type alias DrawableCommon =
    {   x : Float
    ,   y : Float
    ,   alpha : Float
    ,   scale : Float
    ,   sheet : Sheet
    }

defaultDrawable : DrawableCommon
defaultDrawable = { x = 0, y = 0, alpha = 0, scale = 1, sheet = Sprites }

type alias Custom = 
    {   scale : Float
    }

type Drawable =
    CenteredSprite DrawableCommon
    Custom DrawableCommon Custom
    Text DrawableCommon Char

renderDrawList = 


