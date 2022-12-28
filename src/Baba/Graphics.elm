module Baba.Graphics exposing ( Model, Msg, init, update, view, subscription,
                                setGrid )

import Basics exposing ( toFloat, floor )

import Browser.Events
import Dict

import Html.Styled exposing ( div, text )

import Time exposing ( Posix )

import Baba.Cell as Cell
import Baba.LinkedGrid as LinkedGrid exposing ( Direction(..) )
import Baba.Types as Types
import Baba.Util exposing (..)

import Sprites exposing ( Renderable, getAnimatedSprite, getFixedSprite, renderConnectedSprite, renderSprite, renderText )

animDurationMillis = 350
 
--spriteLoaders msg =
--    [ loadFromImageUrl "images/blah.png" (msg << SpritesLoaded)
--    , loadFromImageUrl "images/text.png" (msg << WordsLoaded)
--    ]

cellDimensionInt = 16
cellDimension = toFloat cellDimensionInt

spriteWidth = 24.0
halfSpriteWidth = spriteWidth / 2.0

floorf = floor >> toFloat

-- probably pass this through to all render functions
--gt = transform [scale 2.0 2.0, translate cellDimension (cellDimension/2.0)]

--type alias SpriteFunc = Float -> Float -> Float -> Float -> Texture.Texture -> Canvas.Renderable

--renderSprite : Float -> Float -> Int -> Direction -> SpriteFunc
--renderSprite baseX baseY numFrames direction =
--    let
--        yoff = case direction of
--                    Left -> 0
--                    Right -> 32
--                    Up -> 64
--                    Down -> 96

--        frame delta = Texture.sprite
--            { x = baseX + (if delta > 0.95 then 0 else spriteWidth * floorf (toFloat numFrames * delta))
--            , y = baseY + yoff
--            , width = spriteWidth
--            , height = 32
--            }

--        render delta x y alpha spriteSheet =
--            Canvas.texture
--                [ gt, Canvas.Settings.Advanced.alpha alpha
--                , transform [translate x y]
--                ]
--                ( -halfSpriteWidth, -16 ) ((frame delta) spriteSheet)
--    in
--    render


type alias Box = { x: Float, y: Float, width: Float, height: Float }

--renderNoAnimSprite : Box -> SpriteFunc
--renderNoAnimSprite s scl x y alpha spriteSheet = Canvas.texture
--    [gt, Canvas.Settings.Advanced.alpha alpha
--    , transform (translate x y :: (if scl == 1.0 then 
--        []
--      else [scale scl scl]))
--    ]
--    ( -s.width / 2.0, -s.height / 2.0 ) (Texture.sprite s spriteSheet)

getConnectedSpriteIndices : Cell.Location -> Cell.Object -> ( Int, Int )
getConnectedSpriteIndices loc obj =
    case Cell.getObjectWord obj of
        Cell.Instance noun ->
            let
                hasItemToInt m = case Maybe.map (LinkedGrid.getContents >> (Cell.cellHasNoun noun)) m of
                    Just True -> 1
                    _ -> 0

            in
            ( (LinkedGrid.above loc |> hasItemToInt) + (LinkedGrid.right loc |> hasItemToInt) * 2
            , (LinkedGrid.left loc |> hasItemToInt) + (LinkedGrid.below loc |> hasItemToInt) * 2
            )
        _ ->
            ( 0, 0 )


----sentenceBorderCorners blah = 
    

--renderSentenceBorder : Cell.Location -> Types.RuleHint -> Float -> Texture.Texture -> List Canvas.Renderable
--renderSentenceBorder location hint f t =
--    let
--        ( x, y ) = LinkedGrid.getLocationCoordinates location

--        sprite =
--            case Dict.get '_' instanceSprites of
--                Just s ->
--                    let
--                        above = case hint.vertical of 
--                            Just Types.Middle -> 1
--                            Just Types.End -> 1
--                            _ -> 0

--                        right = case hint.horizontal of 
--                            Just Types.Middle -> 1
--                            Just Types.Start -> 1
--                            _ -> 0

--                        left = case hint.horizontal of 
--                            Just Types.Middle -> 1
--                            Just Types.End -> 1
--                            _ -> 0

--                        below = case hint.vertical of 
--                            Just Types.Middle -> 1
--                            Just Types.Start -> 1
--                            _ -> 0

--                    in
--                    {   x = s.x + (above + right * 2) * s.width
--                    ,   y = s.y + (left + below * 2) * s.height
--                    ,   width = s.width, height = s.height
--                    }

--                _ ->
--                    rockBox
--    in
--    [renderNoAnimSprite sprite 1.0
--        (toFloat x * cellDimension)
--        (toFloat y * cellDimension + 5) f t]



hasWater = Cell.cellHasNoun (Types.Noun 'b')

getWaterSpriteLocations : Int -> Int -> Cell.Location -> List ( Int, Int )
getWaterSpriteLocations baseX baseY loc = 
    let
        relativeLocationHasWater f = f loc |> Maybe.map (LinkedGrid.getContents >> hasWater) |> Maybe.withDefault False
        aboveHas = relativeLocationHasWater LinkedGrid.above
        rightHas = relativeLocationHasWater LinkedGrid.right
        belowHas = relativeLocationHasWater LinkedGrid.below
        leftHas = relativeLocationHasWater LinkedGrid.left

        boolToInt b = if b then 1 else 0

        -- name in terms of top left, but pass in all four pairs
        calc lh ah tlx tly = case ( lh, ah ) of
            ( True, True ) ->
                LinkedGrid.relativeAt tlx tly loc
                    |> Maybe.map LinkedGrid.getContents
                    |> Maybe.map hasWater
                    |> Maybe.withDefault True
                    |> boolToInt >> (+) 3

            _ ->
                boolToInt lh + 2 * boolToInt ah

        xform y x = ( baseX + x * 10, baseY + y * 10 )

    in
    [ calc leftHas  aboveHas -1 -1 |> xform 0
    , calc aboveHas rightHas  1 -1 |> xform 1
    , calc rightHas belowHas  1  1 |> xform 2
    , calc belowHas leftHas  -1  1 |> xform 3
    ]
    


--renderWater : Cell.Location -> Float -> Float -> Float -> Texture.Texture -> List Canvas.Renderable
--renderWater loc x y alpha spriteSheet = 
--    let
--        baseX = 2
--        baseY = 202

--        spriteLocs = getWaterSpriteLocations baseX baseY loc 

--        render ( rx, ry ) ( sx, sy ) = Canvas.texture [gt, Canvas.Settings.Advanced.alpha alpha] ( rx, ry )
--            <| Texture.sprite { x = toFloat sx, y = toFloat sy, width = 8, height = 8 } spriteSheet

--    in
--    case spriteLocs of
--        tl :: tr :: br :: bl :: [] ->
--            [ render ( x - 8, y - 8 ) tl
--            , render ( x, y - 8 ) tr
--            , render ( x, y ) br
--            , render ( x - 8, y ) bl
--            ]

--        _ ->
--            []

rockBox = { x = 0, y = 128, width = 24, height = 32 }

animatedSprites = Dict.fromList
    [ ( 'i', ( 0, 0, 8 ) ) -- link
    , ( 'a', ( 24 * 8, 0, 2 ) ) -- zelda
    ]

getNounSprite sprites noun = case noun of
    Types.Noun c -> Dict.get c sprites

instanceSprites = Dict.fromList
    [ ( 'e',    { x = 232, y = 328, width = 16, height = 16 } ) -- fence
    , ( '_',    { x = 200, y = 232, width = 24, height = 24 } ) -- message box

    , ( 'b',    { x = 232, y = 256, width = 16, height = 16 } ) -- water
    , ( 'c',    { x = 0, y = 128, width = 24, height = 32 } ) -- rock
    , ( 'd',    { x = 48, y = 128, width = 24, height = 32 } ) -- shrub
    , ( 'f',    { x = 24, y = 128, width = 24, height = 32 } ) -- key
    , ( 'g',    { x = 120, y = 128, width = 24, height = 32 } ) -- statue
    , ( 'h',    { x = 96, y = 128, width = 24, height = 32 } ) -- sign
    ]

getInstanceSprite = getNounSprite instanceSprites


getTextSprite text =
    let
        bg =
            case text of
                Types.StativeText _ -> 
                    2

                _ ->
                    0
    in
    {bg = bg, sprite = (Types.getTextInfo text).glyph}


--renderTextBG = renderNoAnimSprite { x = 44, y = 164, width = 16, height = 16 } 1.0
--renderTextBG2 = renderNoAnimSprite { x = 68, y = 164, width = 16, height = 16 } 1.0


--renderMessageBox = renderNoAnimSprite { x = 0, y = 162, width = 40, height = 32 } 0.5


--renderLeft = renderWip 0
--renderRight = renderWip 32
--renderUp = renderWip 64
--renderDown = renderWip 96


setGrid grid model =
    { model
    | grid = Just grid
    , previousGrid = model.grid
    , lastUpdateTime = model.lastAnimTime
    }

type Msg
    = AnimationFrame Posix

type alias Model =
    { grid : Maybe Cell.Grid
    , previousGrid : Maybe Cell.Grid
    , lastAnimTime : Posix
    , lastUpdateTime : Posix
    }

init : Model
init = 
    { grid = Nothing
    , previousGrid = Nothing
    , lastAnimTime = Time.millisToPosix 0
    , lastUpdateTime = Time.millisToPosix 0
    }

update msg model =
    case msg of 

        AnimationFrame time ->
            { model | lastAnimTime = time }

--(( Int, Int, Object ) -> a -> a) -> a -> Grid -> a

--font = Text.font { size = 24, family = "sans-serif" }

-- spriteSheet obj delta animX animY alpha

type alias RenderObjectInfo =
    { obj : Cell.Object
    , delta : Float
    , animX : Float
    , animY : Float
    , alpha : Float
    , location : Maybe Cell.Location
    }

renderObject : RenderObjectInfo -> Renderable
renderObject info =
    let
        x = info.animX * cellDimension
        y = info.animY * cellDimension + 5

        word = Cell.getObjectWord info.obj

    in
    case info.location of
        Just location ->
            case word of
                Cell.Instance noun ->

                    case getAnimatedSprite noun of
                        Just animatedSprite ->
                            renderSprite
                                x y info.alpha info.delta 1
                                (animatedSprite (Cell.getObjectDirection info.obj))
                            --[curry3 renderSprite animatedSprite 
                            --    (Cell.getObjectDirection info.obj) info.delta x y info.alpha info.spriteSheet]

                        _ ->
                            case ( noun, getFixedSprite noun ) of
                                ( Types.Noun objChar, Just fixedSprite ) ->

                                    case objChar of
                                    --    'b' ->
                                    --        []
                                    --        --renderWater location x y info.alpha info.spriteSheet

                                        'e' ->
                                            fixedSprite
                                                |> renderConnectedSprite
                                                    (renderSprite x y info.alpha info.delta 1)
                                                    (getConnectedSpriteIndices location info.obj)

                                        _ ->
                                            renderSprite x y info.alpha info.delta 1 fixedSprite

                                    --        []
                                    --        --[renderNoAnimSprite instanceSprite 1.0 x y info.alpha info.spriteSheet]

                                ( Types.Noun objChar, _ ) ->
                                    renderText x y 1 1 <| String.fromChar objChar
                                    --[Canvas.text [gt, font] ( x - 8, y + 8 ) (String.fromChar objChar)]

                Cell.Text text ->
                    renderText 
                        x y 1 1
                        (Types.getTextInfo text).word
                    --case getTextSprite text of
                    --    { bg, sprite } ->
                    --        [ --(case bg of
                    --          --  0 -> renderTextBG
                    --          --  1 -> renderTextBG2
                    --           --renderMessageBox
                    --           --      x y info.alpha info.spriteSheet
                    --         renderNoAnimSprite sprite 0.6 x y info.alpha info.glyphSheet
                    --        ]

        _ ->
            []


renderGrid dicts delta grid =
    let
        foldFunc : ( Int, Int, Cell.Object ) -> Renderable -> Renderable
        foldFunc ( objX, objY, obj ) acc =
            let

                ( animX, animY, animDelta ) = 
                    case dicts of
                        Just ( prev, _ ) ->
                            case Dict.get (Cell.getObjectId obj) prev of
                                Just ( prevX, prevY, _ ) ->
                                    ( toFloat prevX * (1 - delta) + toFloat objX * delta
                                    , toFloat prevY * (1 - delta) + toFloat objY * delta
                                    , if prevX == objX && prevY == objY then 0 else delta
                                    )

                                _ ->
                                    ( toFloat objX, toFloat objY, 0 )

                        _ ->
                            ( toFloat objX, toFloat objY, 0 )

            in
            renderObject
                { obj = obj
                , delta = animDelta
                , animX = animX
                , animY = animY
                , alpha = 1.0
                , location = LinkedGrid.at objX objY grid
                } ++ acc

        --scl = 0.5 * (1 - delta) + 1.5 * delta
        --messageTest = Canvas.texture
        --    [ gt
        --    , transform [translate 100 100, scale scl scl]
        --    ]
        --    ( -40 / 2.0, -32 / 2.0 ) 
        --    ((Texture.sprite { x = 0, y = 160, width = 40, height = 32 }) spriteSheet)

        gridFoldFunc : (LinkedGrid.Row Cell.Cell) -> Renderable -> Renderable
        gridFoldFunc row acc = 
            let
                borders : Cell.Location -> Renderable -> Renderable
                borders location rowAcc =
                    case (LinkedGrid.getContents location).viewHints of
                        Just hint ->
                            []
                            --renderSentenceBorder location hint 1.0 spriteSheet ++ rowAcc

                        _ ->
                            rowAcc

                rowFoldFunc location rowAcc =
                    let
                        ( x, y ) = LinkedGrid.getLocationCoordinates location


                        textFirst lhs rhs =
                            case ( Cell.getObjectWord lhs, Cell.getObjectWord rhs ) of
                                ( Cell.Text _, Cell.Text _ ) ->
                                    EQ

                                ( _, Cell.Text _) ->
                                    LT

                                ( Cell.Text _, _ ) ->
                                    GT

                                _ ->
                                    EQ

                        cellContents = (LinkedGrid.getContents location).contents
                            |> List.sortWith textFirst
                    in
                    List.foldr (\obj -> foldFunc ( x, y, obj )) rowAcc cellContents

            in
            LinkedGrid.foldRowLocations rowFoldFunc acc row ++
                LinkedGrid.foldRowLocations borders [] row
                

    in
        LinkedGrid.foldRows gridFoldFunc [] grid
        --Cell.foldObjects foldFunc [] grid

millisBetween from to = Time.posixToMillis to - Time.posixToMillis from


gridToIdMap grid = 
    let
        foldFunc ( x, y, obj ) acc = ( Cell.getObjectId obj, ( x, y, obj ) ) :: acc
    in
    Cell.foldObjects foldFunc [] grid
        |> Dict.fromList

-- had  msg: (Msg -> msg)  as first argument (i.e. message converter)
view : (Msg -> msg) -> Model -> Html.Styled.Html msg
view msg model =
    case model.grid of
        Just grid -> 
            let
                --( gridWidth, gridHeight ) = LinkedGrid.getDimensions grid
                --canvasWidth = (gridWidth + 1) * cellDimensionInt * 2
                --canvasHeight = (gridHeight + 1) * cellDimensionInt * 2

                delta = toFloat (millisBetween model.lastUpdateTime model.lastAnimTime)
                            / toFloat animDurationMillis

                objectsInPreviousGrid =
                    if delta < 0 || delta >= 1 then
                        Nothing
                    else
                        case model.previousGrid of
                            Nothing ->
                               Nothing

                            Just prev ->
                                let
                                    prevMap = gridToIdMap prev
                                    gridMap = gridToIdMap grid
                                in
                                    Just ( prevMap, Dict.diff prevMap gridMap )
            
                fadingSprites = 
                    case objectsInPreviousGrid of
                        Just ( _, destroyed ) ->
                                destroyed
                                    |> Dict.toList
                                    |> List.concatMap (\(_, ( x, y, obj )) ->
                                        renderObject
                                        { obj = obj
                                        , delta = 0
                                        , animX = toFloat x
                                        , animY = toFloat y
                                        , alpha = 1.0 - delta
                                        , location = LinkedGrid.at x y grid
                                        })

                        _ ->
                            []

            in

            (renderGrid objectsInPreviousGrid delta grid) ++ fadingSprites
                |> Sprites.toHtml



        _ -> div [] [ text "no grid" ]

--transform [scale 1.0 1.0]
subscription : (Msg -> msg) -> Sub msg
subscription msg = Browser.Events.onAnimationFrame (AnimationFrame >> msg)