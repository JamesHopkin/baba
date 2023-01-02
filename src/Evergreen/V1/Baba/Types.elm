module Evergreen.V1.Baba.Types exposing (..)

type Noun
    = Noun Char


type Conjunction
    = And
    | Not


type LinkingWord
    = Is
    | Has
    | Makes


type Restrictive
    = On
    | Near


type Predicate
    = All
    | Empty
    | Text


type Stative
    = Push
    | Pull
    | Move
    | More
    | Stop
    | Defeat
    | Win
    | Open
    | Shut
    | Float_
    | Hot
    | Melt
    | Sink
    | Shift
    | Weak
    | Tele
    | You


type Text
    = Conjunction Conjunction
    | LinkingWord LinkingWord
    | Restrictive Restrictive
    | PredicateText Predicate
    | StativeText Stative
    | NounText Noun


type RuleHintPart
    = Start
    | Middle
    | End


type alias RuleHint = 
    { horizontal : (Maybe RuleHintPart)
    , vertical : (Maybe RuleHintPart)
    }