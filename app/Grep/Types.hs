module Grep.Types where

type Negative = Bool

data PatternToken = Literal Char
    | Meta MetaType 
    | Group [PatternToken] Negative
    | AnchorStart 
    | AnchorEnd 
    | OptionType
    deriving (Show, Eq)

data MetaType = Word | Digit deriving (Show, Eq)
data OptionType = Plus | Start | Question deriving (Show, Eq)
