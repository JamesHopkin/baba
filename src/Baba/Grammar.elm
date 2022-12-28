module Baba.Grammar exposing (..)

import Baba.Types as Types exposing
    ( textAsSubject, textAsComplement
    )

type alias Complement =
    { word : Types.Complement
    , sense : Bool
    }

complementDebugString compl =
    let
        str = Types.complementDebugString compl.word
    in
    if compl.sense then str else "not " ++ str

type alias Restriction =
    { word : Types.Restrictive
    , noun : Types.Subject
    , sense : Bool
    }

restrictionDebugString restr =
    let
        str = Types.textDebugString (Types.Restrictive restr.word) ++ " " ++ Types.subjectDebugString restr.noun
    in
    if restr.sense then str else "not " ++ str

type Rhs
    = Is (List Complement)
    | Link Types.LinkingWord (List Types.Subject)

type alias Sentence =
    { subject : List Types.Subject
    , restriction : List Restriction
    , rhs : List Rhs
    , start : Int
    , length: Int
    }

-- need a clause function to drop last complement
--  - must be true sense in Is case
--  - must leave at least one complement


{-
    Multiple clause stealing last item
        0  1 2  3  4  5 6
        a is A and b is B
    first clause comes out as 'a is A and b'
    
    steal b to use as 
-}


notes = """


    + means e.g. a+ -->  a [and a...]
    * means e.g. a* --> [a [and a...]]

Complement = not? tcompl
Rhs = (is Complement+) | (link Argument+)
Restriction = not? on|near noun 
Sentence = Subject+ Restriction* Rhs+


    A -> B;
    B -> { B; C; rhs };
    C -> { C; rhs };
    rhs -> { is; link };
    is -> { D; rhs; done };
    link -> { E; rhs; done };

look for:
A: subject
B: subject or restriction or rhs 
C: restriction or rhs
rhs: is+complement or other linking word+argument
D: complement
E: arguments ()

"""

type alias Cursor a =
    {   a
    |   afterLink : List Types.Text
    ,   afterRhs : List Types.Text
    }

parseRhs : List Types.Text -> Maybe (Cursor { rhs : Rhs })
parseRhs words = case words of
    Types.LinkingWord Types.Is :: isTail ->

        case parseComplement isTail of 
            Just ( complement, afterComplement ) ->
                let
                    accumComplements : List Types.Text -> List Complement -> ( List Complement, List Types.Text )
                    accumComplements complWords acc = case complWords of
                        Types.Conjunction Types.And :: afterAnd ->

                            case parseComplement afterAnd of
                                Just ( anotherComplement, remaining ) ->
                                    accumComplements remaining (anotherComplement :: acc)

                                _ ->
                                    ( acc, complWords )

                        _ ->
                            ( acc, complWords )

                    ( complements, afterAllComplements ) = accumComplements afterComplement [complement]
                in
                Just
                    {   rhs = Is complements
                    ,   afterLink = isTail
                    ,   afterRhs = afterAllComplements
                    }

            _ ->
                -- not a complement after "is"
                Nothing

    Types.LinkingWord link :: firstArg :: linkTail ->
        case Types.textAsSubject firstArg of
            Just subject ->
                let
                    accumArguments : List Types.Text -> List Types.Subject -> ( List Types.Subject, List Types.Text )
                    accumArguments argWords acc = case argWords of
                        Types.Conjunction Types.And :: nextArg :: remaining ->
                            case Types.textAsSubject nextArg of
                                Just nextSubject ->
                                    accumArguments remaining (nextSubject :: acc)

                                _ ->
                                    ( acc, remaining )

                        _ ->
                            ( acc, argWords )

                    ( arguments, afterAllArguments ) = accumArguments linkTail [subject]

                in
                Just
                    {   rhs = Link link arguments
                    ,   afterLink = firstArg :: linkTail
                    ,   afterRhs = afterAllArguments
                    }

            _ ->
                -- not a subject after link
                Nothing

    _ ->
        -- no linking word or nothing after link
        Nothing


parseComplement : List Types.Text -> Maybe ( Complement, List Types.Text )
parseComplement words = case words of
    Types.Conjunction Types.Not :: notTerm :: afterNotComplement ->
            case textAsComplement notTerm of
                Just complement ->
                    Just
                        ( { word = complement, sense = False }
                        , afterNotComplement
                        )

                _ ->
                    -- "not" must be followed by complement
                    Nothing

    head :: tail ->
        case textAsComplement head of
            Just complement ->
                Just
                    ( { word = complement, sense = True }
                    , tail
                    )

            _ ->
                -- word after "is" not valid
                Nothing

    _ ->
        -- no word
        Nothing

parseRestriction : List Types.Text -> Maybe ( Restriction, List Types.Text )
parseRestriction words = case words of
    Types.Conjunction Types.Not :: Types.Restrictive restrictive :: firstRestr :: rest ->
        case Types.textAsSubject firstRestr of 
            Just subject ->
                Just
                    ( { word = restrictive, noun = subject, sense = False }
                    , rest
                    )

            _ ->
                Nothing

    Types.Restrictive restrictive :: firstRestr :: rest ->
        case Types.textAsSubject firstRestr of 
            Just subject ->
                Just
                    ( { word = restrictive, noun = subject, sense = True }
                    , rest
                    )
            _ ->
                Nothing

    _ ->
        -- no restrictive
        Nothing

-- A
parse : List Types.Text -> Maybe ( Sentence, List Types.Text )
parse words = 
    case words of 
        head :: tail ->
            case Types.textAsSubject head of

                Just subject ->
                    subjectState [subject] tail

                _ ->
                    Nothing

        _ ->
            Nothing

-- B
subjectState : (List Types.Subject) -> List Types.Text -> Maybe ( Sentence, List Types.Text )
subjectState subjects words = case words of 
    Types.Conjunction Types.And :: head :: tail ->
        case Types.textAsSubject head of
            Just nextSubject ->
                subjectState
                    (nextSubject :: subjects)
                    tail

            _ ->
                -- 'and' not followed by subject: syntax error
                Nothing


    _ ->
        case parseRestriction words of
            Just ( restriction, afterRestriction ) ->
                restrictionState
                    { subject = subjects
                    , restriction = [restriction]
                    }
                    afterRestriction

            _ ->
                case parseRhs words of
                    Just { rhs, afterLink, afterRhs } ->
                        Just <| rhsState
                            {   sentence =
                                { subject = subjects
                                , restriction = []
                                , rhs = [rhs]
                                , start = -1, length = -1
                                }
                            ,   afterLink = afterLink
                            ,   afterRhs = afterRhs
                            }

                    _ ->
                        -- no linking word
                        Nothing

type alias RestrictionStateArg =
    { subject : List Types.Subject
    , restriction : List Restriction
    }

restrictionState : RestrictionStateArg -> List Types.Text -> Maybe ( Sentence, List Types.Text )
restrictionState soFar words = case words of
    Types.Conjunction Types.And :: tail ->
        case parseRestriction tail of
            Just ( restriction, afterRestriction ) ->
                restrictionState
                    { subject = soFar.subject
                    , restriction = restriction :: soFar.restriction
                    }
                    afterRestriction

            _ ->
                -- syntax error: "add" with no restriction
                Nothing

    _ ->
                case parseRhs words of

                    Just { rhs, afterLink, afterRhs } ->
                        Just <| rhsState
                            {   sentence =
                                { subject = soFar.subject
                                , restriction = soFar.restriction
                                , rhs = [rhs]
                                , start = -1, length = -1
                                }
                            ,   afterLink = afterLink
                            ,   afterRhs = afterRhs
                            }

                    _ ->
                        -- no linking word
                        Nothing


-- rhs
rhsState : Cursor { sentence : Sentence } -> ( Sentence, List Types.Text )
rhsState cursor =
    let
        sentenceSoFar = cursor.sentence
        resultSoFar =
            (   { sentenceSoFar
                | length = List.length cursor.afterRhs -- slight hack, stash remaining in here
                }
            ,   cursor.afterLink
            )

    in
    case cursor.afterRhs of
        Types.Conjunction Types.And :: tail ->
            case parseRhs tail of
                Just nextCursor ->
                    rhsState
                        {   sentence =
                                { sentenceSoFar
                                | rhs = nextCursor.rhs :: cursor.sentence.rhs
                                }
                        ,   afterLink = nextCursor.afterLink
                        ,   afterRhs = nextCursor.afterRhs
                        }

                _ ->
                    resultSoFar
        _ ->
            resultSoFar


findSentences : List Types.Text -> List Sentence
findSentences allWords =
    let
        impl words acc =
            case words of
                _ :: tail ->
                    case parse words of
                        Just ( sentence, remaining ) ->
                            let
                                wordsLength = List.length words
                                start = List.length allWords - wordsLength
                                --dummy = Debug.log "sentence" [start, wordsLength]
                                sentenceWithLocation =
                                    {   sentence
                                    |   start = start
                                    ,   length = wordsLength - sentence.length -- see hack above
                                    }
                            in
                            impl remaining (sentenceWithLocation :: acc)

                        _ ->
                            impl tail acc
                _ ->
                    acc
    in
    impl allWords []
