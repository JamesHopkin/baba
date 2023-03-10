module Baba.Cell exposing ( Object, Cell, makeCell, Location, Grid, Axis, emptyCell, moveToCell,
                            objectIs, objectIsAny, cellHas, cellHasAny, cellHasNoun, wordMatchesSubject, objectMatchesSubject,
                            firstText, firstComplement, firstSubject, firstStative, firstLinkingWord,
                            getObjectId, getObjectWord, getObjectDirection, getObjectFlags,
                            makeObject, makeDirectedObject, makeTextObject,
                            setObjectDirection, setObjectFlags, setObjectIs, setObjectWordAndFlags,
                            updateObjectInCell, flipDir, foldObjects,
                            objectDebugChar, cellDebugString, stringListToCells, mismatch,

                            ObjectKind(..) ) -- may encapsulate further

import Dict exposing ( Dict )

import Baba.Types as Types
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )

type alias Cell =
    {   contents : List Object
    ,   viewHints : Maybe Types.RuleHint
    }

makeCell contents = { contents = contents, viewHints = Nothing }
type alias Location = LinkedGrid.Location Cell

type alias Grid = LinkedGrid.LinkedGrid Cell
type alias Axis = LinkedGrid.Axis Cell


type ObjectKind
    = Instance Types.Noun
    | Text Types.Text


type alias ObjectState = 
    { word: ObjectKind
    , direction: Direction
    , flags: Int
    , lastMovedTick: Int
    }

wordMatchesSubject : ObjectKind -> Types.Subject -> Bool
wordMatchesSubject word subject =
    --let
    --    comp = Debug.log "compare" [String.fromChar <| objectKindDebugChar word, Types.subjectDebugString subject]
    --in
    case ( word, subject ) of
        ( Instance noun, Types.NounSubject subjectNoun ) ->
            --let
            --    dummy = Debug.log "compare nouns" [noun, subjectNoun]
            --in
            Types.nounsEqual noun subjectNoun

        ( Text _, Types.Predicate Types.Text ) ->
            True

        ( _, Types.Predicate Types.All ) ->
            -- any exception needed here?
            True

        _ ->
            False

objectMatchesSubject (Object _ state) = wordMatchesSubject state.word


mismatch : Grid -> Grid -> List ( Int, Int )
mismatch a b =
    let
        foldFunc : Location -> List ( Int, Int ) -> List ( Int, Int )
        foldFunc loc acc =
            let
                ( x, y ) = LinkedGrid.getLocationCoordinates loc
            in
            case ( LinkedGrid.at x y a, LinkedGrid.at x y b ) of
                ( Just cellA, Just cellB ) ->
                    let
                        sortedDebugChars : Location -> List Char
                        sortedDebugChars = LinkedGrid.getContents >> (.contents) >> List.map objectDebugChar >> List.sort
                    in
                    if compare (sortedDebugChars cellA) (sortedDebugChars cellB) == EQ then
                        acc 
                    else
                        --let
                        --    lengthsLog = Debug.log "lengths" [List.length (sortedDebugChars cellA), List.length (sortedDebugChars cellB)]
                        --    idsLog = Debug.log "chars" (sortedDebugChars cellA ++ sortedDebugChars cellB)
                        --in
                        ( x, y ) :: acc

                _ ->
                    acc

    in

    LinkedGrid.foldLocations foldFunc [] a



type Object = Object Int ObjectState

emptyCell : Cell
emptyCell = makeCell []



clearCellAt offset = LinkedGrid.axisSetAt offset []
clearCell = clearCellAt 0

-- use id to pick direction
makeObject id c =
    let
        dir = case remainderBy 4 id of
            0 -> Up
            1 -> Right
            2 -> Down
            _ -> Left
    in
    Object id
        { word = Instance (Types.Noun c)
        , direction = dir
        , flags = 0
        , lastMovedTick = -1
        }

makeDirectedObject id c direction =
    Object id
        { word = Instance (Types.Noun c)
        , direction = direction
        , flags = 0
        , lastMovedTick = -1
        }

makeTextObject id text =
    let
        dir = case remainderBy 4 id of
            0 -> Up
            1 -> Right
            2 -> Down
            _ -> Left
    in
    Object id
        { word = Text text
        , direction = dir
        , flags = 0
        , lastMovedTick = -1
        }

getObjectId object = case object of
    Object id _ -> id

getObjectWord object = case object of
    Object _ state -> state.word

getObjectDirection object = case object of
    Object _ state -> state.direction

setObjectDirection direction object = case object of
    Object id state ->
        Object id 
            { state
            | direction = direction
            }

getObjectFlags object = case object of
    Object id state -> state.flags

setObjectFlags flags object = case object of
    Object id state ->
        Object id 
            { state
            | flags = flags
            }

setObjectWordAndFlags word flags object = case object of
    Object id state ->
        Object id
            { state
            | word = word
            , flags = flags
            }

setObjectIs verb object = case object of
    Object id state ->
        Object id 
            { state
            | flags = Types.flagFor verb
            }

addToCellAt offset object axis =
    let
        newContents = object :: (LinkedGrid.axisGetAt offset axis)
    in
        LinkedGrid.axisSet newContents axis
addToCell = addToCellAt 0

updateObjectInCell : Object -> Location -> Location
updateObjectInCell updatedObject loc =
    let
        id = getObjectId updatedObject
        replace cell =
            { cell | contents = updatedObject :: List.filter (getObjectId >> (/=) id) cell.contents }

    in
        LinkedGrid.getContents loc
            |> replace
            |> (\new -> LinkedGrid.setContents new loc)


moveToCell : List Int -> Int -> Int -> Maybe Direction -> Axis -> Maybe Axis
moveToCell ids from to maybeDirection axis =
    let
        fromContent = (LinkedGrid.axisGetAt from axis).contents

        shouldMoveFrom obj = List.member (getObjectId obj) ids

        --dummy0 = Debug.log "move from content" fromContent
        toMove = List.filter shouldMoveFrom fromContent
        --dummy = Debug.log "move ids" [id, from, to, if isJust maybeObj then 1 else 0]

    
        result = 
            if List.isEmpty toMove then
                Nothing

            else
                let
                    movedItems = case maybeDirection of
                        Just newDirection ->
                            List.map (setObjectDirection newDirection) toMove

                        _ ->
                            toMove

                    newFromContent = makeCell <| List.filter (not << shouldMoveFrom) fromContent
                    newToContent = makeCell <| movedItems ++ (LinkedGrid.axisGetAt to axis).contents
                in
                    Just
                        ( axis
-- could optimise to not replace grid twice
                            |> LinkedGrid.axisSetAt from newFromContent
                            |> LinkedGrid.axisSetAt to newToContent
                        )

        --dummy3 = Debug.log "move (after)"
        --  ( case result of 
        --    Just newAxis -> 
        --      [ LinkedGrid.axisOrigin newAxis |> LinkedGrid.getLocationCoordinates |> (\( x, y ) -> String.fromInt x ++ ", " ++ String.fromInt y)
        --      , LinkedGrid.axisGetAt -1 newAxis |> cellDebugString
        --      , LinkedGrid.axisGetAt 0 newAxis |> cellDebugString
        --      , LinkedGrid.axisGetAt 1 newAxis |> cellDebugString
        --      ]
        --    _ -> ["failed"]
        --  )
    in
    result

foldObjects : (( Int, Int, Object ) -> a -> a) -> a -> Grid -> a
foldObjects f acc grid =
    let
        foldFunc : Location -> a -> a
        foldFunc location innerAcc =
            let
                ( x, y ) = LinkedGrid.getLocationCoordinates location
            in
            List.foldr (\obj -> f ( x, y, obj )) innerAcc (LinkedGrid.getContents location).contents
    in
    LinkedGrid.foldLocations foldFunc acc grid

flipDir : Object -> Object
flipDir object = case object of
    Object id state ->
        Object id 
            { state
            | direction = LinkedGrid.flipDir state.direction
            }

objectIs : Types.Stative -> Object -> Bool
objectIs stative object = Types.is stative (getObjectFlags object)

objectIsAny : List Types.Stative -> Object -> Bool
objectIsAny statives object = Types.isAny statives (getObjectFlags object)

cellHas : Types.Stative -> Cell -> Bool
cellHas stative cell = List.any (objectIs stative) cell.contents

cellHasNoun : Types.Noun -> Cell -> Bool
cellHasNoun noun cell = 
    let
        isNoun obj = case getObjectWord obj of
            Instance objNoun ->
                Types.nounsEqual noun objNoun

            _ ->
                False
    in
    List.any isNoun cell.contents

cellHasAny : List Types.Stative -> Cell -> Bool
cellHasAny statives cell = List.any (objectIsAny statives) cell.contents

firstText : Cell -> Maybe Types.Text
firstText cell =
    let
        asText : Object -> Maybe Types.Text
        asText obj = 
            let
                word = getObjectWord obj
            in
            case word of 
                Text text -> 
                    Just text

                _ ->
                    Nothing
    in
    cell.contents
        |> List.filterMap asText
        |> List.head


firstLinkingWord : Cell -> Maybe Types.LinkingWord
firstLinkingWord cell =
    let
        asLinkingWord : Object -> Maybe Types.LinkingWord
        asLinkingWord obj = case getObjectWord obj of
            Text (Types.LinkingWord word) -> Just word
            _ -> Nothing
    in
    cell.contents
        |> List.filterMap asLinkingWord
        |> List.head

firstComplement : Cell -> Maybe Types.Complement
firstComplement cell =
    let
        asComplement : Object -> Maybe Types.Complement
        asComplement obj = case getObjectWord obj of
            Text text -> Types.textAsComplement text
            _ -> Nothing
    in
    cell.contents
        |> List.filterMap asComplement
        |> List.head

firstSubject : Cell -> Maybe Types.Subject
firstSubject cell =
    let
        asSubject : Object -> Maybe Types.Subject
        asSubject obj = case getObjectWord obj of
            Text text -> Types.textAsSubject text
            _ -> Nothing
    in
    cell.contents
        |> List.filterMap asSubject
        |> List.head

firstStative : Cell -> Maybe Types.Stative
firstStative cell =
    let
        asStative : Object -> Maybe Types.Stative
        asStative obj = case getObjectWord obj of
            Text text -> 
                case text of
                    Types.StativeText stative -> Just stative
                    _ -> Nothing
            _ -> Nothing
    in
    cell.contents
        |> List.filterMap asStative
        |> List.head




showIds = False
showAllContents = False
showDirections = False

objectDebugChar : Object -> Char
objectDebugChar object = objectKindDebugChar (getObjectWord object)

objectKindDebugChar : ObjectKind -> Char
objectKindDebugChar kind =
    case kind of 
        Instance (Types.Noun c) -> c
        Text (Types.NounText (Types.Noun c)) -> Char.toUpper c
        Text (Types.StativeText stative) -> 
            case stative of
                Types.Sink -> 'K'
                Types.Pull -> 'L'
                Types.Move -> 'M'
                Types.Stop -> 'S'
                Types.Hot -> 'O'
                Types.Push -> 'P'
                Types.Defeat -> 'T'
                Types.Open -> 'U'
                Types.Shut -> 'V'
                Types.Win -> 'W'
                Types.You -> 'Y'
                Types.Melt -> 'Z'
                _ -> '??'

        Text (Types.PredicateText Types.Text) -> 'X'
        Text (Types.LinkingWord Types.Is) -> '='
        Text (Types.LinkingWord Types.Has) -> '<'

        Text (Types.Conjunction Types.And) -> '&'
        Text (Types.Conjunction Types.Not) -> '!'
        Text (Types.Restrictive Types.On) -> '_'
        _ -> ','


cellDebugString : Cell -> String
cellDebugString cell =
    if showIds then
        List.map (getObjectId >> String.fromInt) cell.contents
         |> String.join ""
    else if showDirections then 
        Maybe.withDefault "?? " (Maybe.map
            (\obj -> String.fromChar (objectDebugChar obj) ++ (case getObjectDirection obj of
                Up -> "???"
                Right -> "???"
                Down -> "???"
                Left -> "???"
            ))
            (List.head cell.contents)
            )

    else
        let
            toString c = String.fromChar (if c == '??' then '!' else c)
            chars = List.map objectDebugChar cell.contents
        in
            if showAllContents then
                case chars of
                    [] -> "??"
                    _ -> String.fromList chars

            else
                case chars of
                    first :: second :: _ -> toString first ++ toString second
                    first :: _           -> toString first ++ " "
                    _ -> "?? "

nextIndex : Char -> Dict Char Int -> ( Int, Dict Char Int )
nextIndex c indices =
    let
        index = Maybe.withDefault 0 (Dict.get c indices) + 1
    in
    ( index + Char.toCode c * 1000, Dict.insert c index indices )

stringListToCells : List String -> List (List Cell)
stringListToCells rows =
    let
        cellFoldFunc : Char -> ( Dict Char Int, List Cell ) -> ( Dict Char Int, List Cell )
        cellFoldFunc c ( indices, outRow ) =
            let
                ( id, newIndices ) = nextIndex c indices
            in
            if c == ' ' then
                ( newIndices, emptyCell :: outRow )

            -- multi object test
            else if c == '@' then
                ( newIndices, 
                    makeCell [ makeObject id 'b'
                    , makeObject (id + 100000) 'c'
                    ] :: outRow
                )

            else if c == '??' then
                ( newIndices, 
                    makeCell [ makeObject id 'a'
                    , makeObject (id + 100000) 'b'
                    , makeObject (id + 200000) 'c'
                    ] :: outRow
                )

            else
                let newObject = case c of

                        '???' ->
                            makeDirectedObject id 'a' Up

                        '???' ->
                            makeDirectedObject id 'a' Right

                        '???' ->
                            makeDirectedObject id 'a' Down

                        '???' ->
                            makeDirectedObject id 'a' Left

                        _ ->
                            case Dict.get c Types.textByCode of
                                Just text ->
                                    makeTextObject id text

                                _ ->
                                    makeObject id c
                in
                ( newIndices, makeCell [newObject] :: outRow )

        makeRow : String -> ( Dict Char Int, List (List Cell) ) -> ( Dict Char Int, List (List Cell) )
        makeRow s ( initialIndices, outRows ) =
                s
                    |> String.toList
                    |> List.foldr cellFoldFunc ( initialIndices, [] )
                    |> \( index, cells ) -> ( index, cells :: outRows)

    in
        --List.foldr (String.toList >> (List.foldr makeCell) >> Tuple.second) ( 0, [] ) rows
        List.foldr makeRow ( Dict.empty, [] ) rows |> Tuple.second
