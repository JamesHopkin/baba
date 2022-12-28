module Baba.Rules exposing ( isTransform,
                            rulesFromSentence, ruleDebugString, RulesResult, RuleSource, RuleDirection (..),
                            lookForRules, getApplicableStative, getApplicableTransform,
                            Rule )

import Baba.Cell as Cell exposing (..)
import Baba.Grammar as Grammar
import Baba.LinkedGrid as LinkedGrid exposing ( Direction (..) )
import Baba.Types as Types

import Baba.Util exposing (..)

type alias RuleSource = { x : Int, y : Int, length : Int, direction: RuleDirection }

type alias RulesResult =
    {   positive : List Rule
    ,   negative : List Rule
    ,   positions : List RuleSource -- just list of where they all are at the moment, enough for view
    }

type RuleDirection = Horizontal | Vertical

emptyResults = { positive = [], negative = [], positions = [] }

add lhs rhs =
    {   positive = lhs.positive ++ rhs.positive
    ,   negative = lhs.negative ++ rhs.negative
    ,   positions = lhs.positions ++ rhs.positions
    }

concatMap : (a -> RulesResult) -> List a -> RulesResult
concatMap f list = 
    List.foldr add emptyResults (List.map f list)

lookForRulesOnAxis : RuleSource -> Axis -> RulesResult
lookForRulesOnAxis source startingAxis = 
    let
        impl : Maybe Axis -> List Types.Text -> Int -> RulesResult -> RulesResult
        impl axis current pos complete =
            let
                getText a = firstText (LinkedGrid.axisGet a)
                next = Maybe.andThen (LinkedGrid.axisForward 1) axis
            in
            case Maybe.andThen getText axis of
                    Just text ->
                        impl next (text :: current) (pos + 1) complete

                    _ ->
                        let
                            rules =
                                let
                                    currentLength = List.length current
                                    start = pos - currentLength

                                    adjust sentence =
                                        {   sentence
                                        |   start = sentence.start + start
                                        }
                                in
                                if List.length current > 0 then
                                    Grammar.findSentences (List.reverse current)
                                        |> List.map adjust
                                        |> concatMap (rulesFromSentence source)
                                        |> add complete
                                else
                                    --let
                                    --    ( p, n ) = complete
                                    --    dummy = Debug.log "on axis" [List.length p, List.length n]
                                    --in
                                    complete
                        in
                        if isJust axis then
                            impl next [] (pos + 1) rules
                        else
                            rules
    in
    impl (Just startingAxis) [] 0 emptyResults


fold :
    (loc -> Maybe loc)
    -> (loc -> acc -> acc)
    -> loc
    -> acc -> acc
fold nextFn f loc acc =
    let
        soFar = f loc acc
    in
        case nextFn loc of
            Just next -> fold nextFn f next soFar
            _ -> soFar

lookForRules : Grid -> RulesResult
lookForRules grid =
    case LinkedGrid.at 0 0 grid of
        Just origin ->
            let
                rowRules : RulesResult
                rowRules = 
                    let
                        rowFunc : Location -> ( Int, RulesResult ) -> ( Int, RulesResult )
                        rowFunc loc ( index, acc ) = 
                            (   index + 1
                            ,   LinkedGrid.makeAxis loc Right
                                    |> lookForRulesOnAxis { x = 0, y = index, direction = Horizontal, length = -1 }
                                    |> add acc
                            )

                        rowResults = fold LinkedGrid.below rowFunc origin ( 0, emptyResults )
                            |> Tuple.second

                        textIsPush = Is (Types.Predicate Types.Text) [] (Types.Stative Types.Push)
                    in
                    {   rowResults
                    |   positive = textIsPush :: rowResults.positive
                    }


                columnRules : RulesResult
                columnRules = 
                    let
                        columnFunc loc ( index, acc ) =
                            (   index + 1
                            ,   LinkedGrid.makeAxis loc Down
                                    |> lookForRulesOnAxis { x = index, y = 0, direction = Vertical, length = -1 }
                                    |> add acc
                            )
                    in
                    fold LinkedGrid.right columnFunc origin ( 0, emptyResults )
                        |> Tuple.second

            in
                add rowRules columnRules

        _ -> emptyResults

type Rule
    = Is Types.Subject (List Grammar.Restriction) Types.Complement
    | Link Types.LinkingWord Types.Subject (List Grammar.Restriction) Types.Subject

ruleDebugString sense rule = 
    let

        subjectAndRestrictions s r = 
            Types.subjectDebugString s :: (if List.length r == 0 then [] else [
                String.join " and " (List.map Grammar.restrictionDebugString r)
            ])
    in
    case rule of
        Is s r c -> String.join " " <| subjectAndRestrictions s r ++ [if sense then "is" else "is not", Types.complementDebugString c]
        Link l s r o -> String.join " " <| subjectAndRestrictions s r ++ [Types.textDebugString (Types.LinkingWord l), Types.subjectDebugString o]

getRestrictions : Rule -> List Grammar.Restriction
getRestrictions rule = case rule of
    Is _ restrictions _
        -> restrictions

    Link _ _ restrictions _
        -> restrictions

rulesFromSentence : RuleSource -> Grammar.Sentence -> RulesResult
rulesFromSentence source sentence =
    let
        subjectFold : Types.Subject -> RulesResult -> RulesResult
        subjectFold subject acc =
            let 
                rhsFold : Grammar.Rhs -> RulesResult -> RulesResult
                rhsFold rhs rhsAcc = 
                    case rhs of
                        Grammar.Is complements ->
                            let
                                complementFold : Grammar.Complement -> RulesResult -> RulesResult
                                complementFold compl complAcc =
                                    let
                                        rule = Is subject sentence.restriction compl.word
                                        --dummy = Debug.log "rules" [sentence.start, sentence.length]

                                        positions = position :: complAcc.positions
                                    in
                                    if compl.sense then
                                        {   complAcc
                                        |   positive = rule :: complAcc.positive
                                        }
                                    else
                                        {   complAcc
                                        |   negative = rule :: complAcc.negative
                                        }
                            in
                                List.foldr complementFold rhsAcc complements

                        Grammar.Link link arguments ->
                            {   rhsAcc
                            |   positive = rhsAcc.positive ++ List.map (Link link subject sentence.restriction) arguments
                            }

            in
            List.foldr rhsFold acc sentence.rhs

        position =
            case source.direction of
                Horizontal ->
                    {   source
                    |   x = sentence.start
                    ,   length = sentence.length
                    }

                Vertical ->
                    {   source
                    |   y = sentence.start
                    ,   length = sentence.length
                    }

        result = List.foldr subjectFold emptyResults sentence.subject
    in
    { result | positions = [position] }

passesRestriction : Grammar.Restriction -> Cell -> Bool
passesRestriction restriction cell =
    case restriction.word of
        Types.On ->
            case restriction.noun of
                Types.Predicate Types.Empty ->
                    List.length cell.contents == 1

                _ ->
                    List.any (\obj -> Cell.objectMatchesSubject obj restriction.noun) cell.contents
        _ ->
            -- only handling On so far
            True

checkRestrictions restrictions cell result =
    let
        foldFunc r acc =
            if isJust acc && passesRestriction r cell then
                acc

            else
                Nothing
    in
    List.foldr foldFunc (Just result) restrictions

-- will need neighbouring cells in the end
getApplicableStative : Rule -> Cell -> Object -> Maybe Types.Stative
getApplicableStative rule cell object =
    case rule of
        Is subject restrictions (Types.Stative stative) ->
            if wordMatchesSubject (Cell.getObjectWord object) subject then
                checkRestrictions restrictions cell stative
            else
                Nothing

        _ ->
            Nothing

isTransform rule = case rule of
    Is _ _ (Types.Stative _) ->
        False

    Is _ _ _ ->
        True

    _ ->
        False

getApplicableTransform : Rule -> Cell -> Object -> Maybe Cell.ObjectKind
getApplicableTransform rule cell object =
    case rule of
        Is subject restrictions complement ->
            if wordMatchesSubject (Cell.getObjectWord object) subject then
                case ( subject, complement ) of
                    ( _, Types.NounComplement noun ) ->
                        checkRestrictions restrictions cell (Cell.Instance noun)

                    ( Types.NounSubject noun, Types.PredicateComplement Types.Text ) ->
                        checkRestrictions restrictions cell <| Cell.Text <| Types.NounText noun

                    _ ->
                        Nothing

            else
                Nothing

        _ ->
            Nothing
