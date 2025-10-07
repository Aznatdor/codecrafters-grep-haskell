module Grep.Types where

type Negative = Bool

data PatternToken = Literal Char
    | Meta MetaType 
    | Group [PatternToken] Negative
    | AnchorStart 
    | AnchorEnd 
    | Star | Plus | Question                -- Option types for  preproccessing
    | Repeater PatternToken Int (Maybe Int) -- for matching
    deriving (Show, Eq)

data MetaType = Word | Digit deriving (Show, Eq)
