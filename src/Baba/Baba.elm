module Baba.Baba exposing ( Model, Msg(..), init, update, subscription,
                            turn, countChars, replaceGrid, gridFromString,
                            nonGraphicsUpdate, updateGraphics, SingleKeyOp(..),
                            keyDecoder )
-- remember to remove exposure of Msg constructors!

import Bitwise

import Dict exposing ( Dict )
import Json.Decode as Decode
import Browser.Events

import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Cell as Cell
import Baba.Destroy as Destroy
import Baba.Graphics as Graphics
import Baba.Make as Make
import Baba.Move as Move
import Baba.Rules as Rules
import Baba.Types as Types

import Baba.Util exposing (..)

{- little bit of thought needed for moving multiple
    some cases:
        multiple things pushing in same direction
            - first will push stuff, rest will move without pushing
        multiple things moving in different directions
            - some might not be able to move, so need to keep track
            - set new contents to be those that were stuck

    Ignore stuck ones in first attempt, i.e. just clear 
-}





--maybeAxisToString : Maybe Axis -> String
--maybeAxisToString axis =
--  axis
--    |> Maybe.map LinkedGrid.gridFromAxis
--    |> Maybe.map (LinkedGrid.toDebugString cellDebugString)
--    |> Maybe.withDefault "!" 

applyRules : Cell.Grid -> ( Rules.RulesResult, Cell.Grid )
applyRules grid =
    let
        rules = Rules.lookForRules grid

        foldFunc : ( Int, Int, Cell.Object ) -> Cell.Grid -> Cell.Grid
        foldFunc ( x, y, object ) gridToUpdate =
            case LinkedGrid.at x y gridToUpdate of
                Just location ->
                    let
                        cell = LinkedGrid.getContents location

                        ruleFoldFunc : Rules.Rule -> Int -> Int
                        ruleFoldFunc rule flags =
                            case Rules.getApplicableStative rule cell object of
                                Just stative ->
                                    Bitwise.or flags (Types.flagFor stative)

                                Nothing ->
                                    flags

                        -- not considering negatives yet!
                        calculatedFlags = List.foldr ruleFoldFunc 0 rules.positive
                    in
                        if calculatedFlags == Cell.getObjectFlags object then
                            gridToUpdate
                        else
                            let
                                updatedObject = Cell.setObjectFlags calculatedFlags object
                            in
                            Cell.updateObjectInCell updatedObject location
                                |> LinkedGrid.gridFromLocation
                _ ->
                    LinkedGrid.make Cell.emptyCell 0 0

    in
    ( rules, Cell.foldObjects foldFunc grid grid )


applyViewHints : Cell.Grid -> Cell.Grid
applyViewHints grid =
    let
        -- should pass in rules
        result = Rules.lookForRules grid

        -- fold rules, 
        getHint : Int -> Int -> Types.RuleHint
        getHint x y =
            let
                hintFoldFunc : Rules.RuleSource -> Types.RuleHint -> Types.RuleHint
                hintFoldFunc source soFar =
                    let
                        hintValue n start = 
                            --let
                                --dummy7 = Debug.log "hintVal" [x, y, n, start, source.length]
                            --in

                            if n < start || n >= start + source.length then Nothing
                            else if n == start then Just Types.Start
                            else if n == start + source.length - 1 then Just Types.End
                            else Just Types.Middle

                        combine existing new =
                            --let
                            --    comblog = Debug.log "comb" [existing, new]
                            --in
                            case ( existing, new ) of
                                ( _, Nothing ) -> existing
                                ( Nothing, _ ) -> new
                                _ -> Just Types.Middle


                    in
                    case source.direction of
                        Rules.Horizontal -> 
                            if source.y == y then
                                { soFar | horizontal = combine soFar.horizontal (hintValue x source.x) }

                            else
                                soFar

                        Rules.Vertical ->
                            if source.x == x then
                                { soFar | vertical = combine soFar.vertical (hintValue y source.y) }

                            else
                                soFar


            in
            List.foldr hintFoldFunc { horizontal = Nothing, vertical = Nothing } result.positions

        foldFunc : Cell.Location -> Cell.Grid -> Cell.Grid
        foldFunc location acc =
            let
                ( x, y ) = LinkedGrid.getLocationCoordinates location

                hint = getHint x y
                maybeHint = if isNothing hint.horizontal && isNothing hint.vertical then Nothing
                    else
                        --let
                        --    dummy1 = Debug.log "viewHints" [x, y]
                        --    dummy2 = Debug.log "viewHints" [hint]
                        --in
                        Just hint
            in
            -- dance to update latest grid (acc) not initial grid reference by location
            case LinkedGrid.at x y acc of
                Just loc ->
                    let
                        cell = LinkedGrid.getContents loc
                    in
                    LinkedGrid.setContents { cell | viewHints = maybeHint } loc
                        |> LinkedGrid.gridFromLocation

                _ ->
                    -- shouldn't happen
                    acc

    in
    LinkedGrid.foldLocations foldFunc grid grid

    --let
    --    shouldMove obj =
    --        if Cell.objectIs Types.Move obj then
    --            Just (Cell.getObjectDirection obj)

    --        else
    --            Nothing

    --    chainDestroys maybeGrid =
    --        case maybeGrid of
    --            Just grid ->

    --in
    --grid
    --    |> applyRules
    --    |> Move.doMovesAndPushes shouldMove True
    --    |> Maybe.map Destroy.doDestroys
    --    |> Maybe.withDefault grid

turnImpl : Bool -> Maybe Direction -> Cell.Grid -> Maybe Cell.Grid
turnImpl forceTransform youDirection currentGrid =
    let

        -- rules
        ( _, gridWithUpToDateRules ) = applyRules currentGrid

        -- you
        ( numberOfYousMoved, afterYousMoved ) =
            case youDirection of
                Just direction ->
                    let
                        youMoveFunc obj = 
                            if Cell.objectIs Types.You obj then
                                Just direction 
                            else
                                Nothing
                    in
                    Move.doMovesAndPushes youMoveFunc False gridWithUpToDateRules

                _ ->
                    ( 0, gridWithUpToDateRules )

        -- moves
        shouldMove obj =
            if Cell.objectIs Types.Move obj then
                Just (Cell.getObjectDirection obj)

            else
                Nothing

        ( numberOfOthersMoved, afterAllMoves ) = Move.doMovesAndPushes shouldMove True afterYousMoved

        --dummy = Debug.log "move counts" [numberOfYousMoved, numberOfOthersMoved]

        transform rules grid =
            let
                justTransforms = List.filter Rules.isTransform
                transformRules = ( justTransforms rules.positive, justTransforms rules.negative )
            in
            if List.isEmpty (Tuple.first transformRules) then
                grid
            else
                Make.doTransformations transformRules grid

    in
    if forceTransform || numberOfYousMoved + numberOfOthersMoved > 0 then
        afterAllMoves
            |> applyRules |> Tuple.second
            |> Destroy.doDestroys 
            |> applyRules
            |> curry2 transform
            |> applyViewHints
            |> Just
    else
        Nothing

updateGraphics : Model -> Model
updateGraphics model = 
    case List.head model.undoStack of
        Just grid ->
            { model
            | graphics = Graphics.setGrid grid model.graphics
            }
     
        _ ->
            model


turn : Maybe Direction -> List Cell.Grid -> Maybe (List Cell.Grid)
turn maybeDirection undoStack =
    case undoStack of
        currentGrid :: _ ->
            turnImpl (List.length undoStack == 1) maybeDirection currentGrid
                |> Maybe.map (\newGrid -> newGrid :: undoStack)

        _ ->
            Nothing

turnAndUpdateGraphics : Maybe Direction -> Model -> Model
turnAndUpdateGraphics maybeDirection model =
    case turn maybeDirection model.undoStack of
        Just updatedStack -> 
            { model
            | undoStack = updatedStack
            }
            |> updateGraphics

        _ ->
            model


countChars grid = 
    let
        addContentsToDict : Cell.Object -> Dict Char Int -> Dict Char Int
        addContentsToDict obj dict =
            let
                key = Cell.objectDebugChar obj
                count = Maybe.withDefault 0 (Dict.get key dict)
            in
            Dict.insert key (count + 1) dict

        addLocationsToDict : Cell.Location -> Dict Char Int -> Dict Char Int
        addLocationsToDict loc counts =
            List.foldr addContentsToDict counts (LinkedGrid.getContents loc).contents
    in
    LinkedGrid.foldLocations addLocationsToDict Dict.empty grid
        |> Dict.toList



-- keyboard
keyDecoder : (Msg -> msg) -> Decode.Decoder msg
keyDecoder msg =
    Decode.map (interpretKey >> msg) (Decode.field "key" Decode.string)


interpretKey : String -> Msg
interpretKey string =
    case String.uncons string of
        Just ( 'w', "" ) -> MoveYou Up
        Just ( 'd', "" ) -> MoveYou Right
        Just ( 's', "" ) -> MoveYou Down
        Just ( 'a', "" ) -> MoveYou Left
        Just ( 'u', "" ) -> SingleKey Undo
        Just ( 'z', "" ) -> SingleKey Wait

        _ ->
            SingleKey Ignore


type SingleKeyOp = Undo | Wait | Ignore

type Msg
    = MoveYou Direction
    | SingleKey SingleKeyOp
    | GraphicsMsg Graphics.Msg

type alias Model =
    { undoStack : List Cell.Grid
    , debugStr : String
    , graphics : Graphics.Model
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

gridFromString str = 

    let
        dropEmptyLines x =
            case x of
                "" :: rest ->
                    dropEmptyLines rest

                _ ->
                    x

        lines = dropEmptyLines (String.split "\n" str)

        width =
            lines
            |> List.map String.length
            |> List.maximum
            |> Maybe.withDefault 0
            |> max 10

        height = max 10 (List.length lines)

    in
    lines
        |> Cell.stringListToCells
        |> LinkedGrid.fromLists Cell.emptyCell width height


initialModel str = 
    { undoStack = [gridFromString str]
    , debugStr = ""
    , graphics = Graphics.init 
    }
    |> updateGraphics

replaceGrid str model = 
    { undoStack = [gridFromString str]
    , debugStr = ""
    , graphics = model.graphics
    }
    |> updateGraphics


init : (Msg -> msg) -> ( Model, Cmd msg )
init _ = ( initialModel initialGridStr, Cmd.none )

nonGraphicsUpdate : Msg -> List Cell.Grid -> ( Maybe String, Maybe (List Cell.Grid) )
nonGraphicsUpdate msg undoStack =
    case msg of
        MoveYou direction ->
            let
                updatedStack = turn (Just direction) undoStack

                -- should get updated on undo too!
                debugStr = case updatedStack of
                    Just (grid :: _) -> 
                        let
                            rules = Rules.lookForRules grid

                            ruleStrs = 
                                (rules.positive |> List.map (Rules.ruleDebugString True)) ++
                                (rules.negative |> List.map (Rules.ruleDebugString False))
                        
                        in
                        String.join ", " ruleStrs
                    _ -> ""
            in
            ( Just debugStr, updatedStack )

        SingleKey Undo ->
            case undoStack of
                _ :: _ :: _ ->
                    -- should update debug string here
                    ( Nothing, Just (List.drop 1 undoStack) )

                _ ->
                    ( Nothing, Nothing )

        SingleKey Wait -> 
            ( Nothing, turn Nothing undoStack )

        _ ->
            ( Nothing, Nothing )


update : Msg -> Model -> Model 
update msg model =
    case msg of
        GraphicsMsg graphicsMsg -> 
            { model | graphics = Graphics.update graphicsMsg model.graphics }

        _ ->
            let
                ( debugStr, updatedStack ) = nonGraphicsUpdate msg model.undoStack
            in
            case updatedStack of
                Just stack ->
                    { model
                    | undoStack = stack
                    , debugStr = Maybe.withDefault model.debugStr debugStr
                    }

                _ ->
                    model

subscription : (Msg -> msg) -> Sub msg
subscription msg = 
    Sub.batch
        [ Browser.Events.onKeyDown (keyDecoder msg)
        , Graphics.subscription (GraphicsMsg >> msg)
        ]
