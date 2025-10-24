module Grep.Types where

type Negative = Bool
type GroupNum = Int

data PatternToken = Literal Char
    | Meta MetaType 
    | Group [PatternToken] Negative
    | AnchorStart 
    | AnchorEnd 
    | Star | Plus | Question                -- Option types for  preproccessing
    | Repeater PatternToken Int (Maybe Int) -- for matching
    | Quantifier Int (Maybe Int)            -- For preprocessing {n,m} type of quantifiers
    | Wildcard
    | Alteration GroupNum [[PatternToken]]
    | Backreference GroupNum
    deriving (Show, Eq)

data MetaType = Word | Digit deriving (Show, Eq)
