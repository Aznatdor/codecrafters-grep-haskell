module Grep.Matcher (mainMatch) where

import Data.Char (isDigit, isLetter)
import Data.List (findIndex, tails)
import Grep.Types
import Grep.Parser

-- Useful synonims
type Pattern = String
type Input = String

isWord :: Char -> Bool
isWord c = c `elem` '_':['a'..'z']++['A'..'Z']++['0'..'9']

-- tries to find a match within input string
mainMatch :: Pattern -> Input -> Bool
mainMatch rawPattern input
    | head pattern == AnchorStart = matchPattern (tail pattern) input
    | otherwise = any (matchPattern pattern) $ tails input
    where pattern = (applyRepeat . parsePattern 1) rawPattern

-- matches repeated token and returns all possible input tails after matching
-- For example: repeatMatches "a" 1 3 "aaaab" -> ["aaab", "aab", "ab"]
repeatMatches :: PatternToken -> Int -> Maybe Int -> String -> [String]
repeatMatches _ _ _ [] = [[]]
repeatMatches token minRep Nothing input
    | matchPattern testPattern testInput = drop minRep input : repeatMatches token (minRep+1) Nothing input
    | otherwise = [] -- if no match, return empty array
    where testPattern = replicate minRep token
          testInput  = take minRep input
repeatMatches token minRep (Just maxRep) input
    | minRep <= maxRep && matchPattern testPattern testInput = drop minRep input : repeatMatches token (minRep+1) (Just maxRep) input
    | otherwise = []
    where testPattern = replicate minRep token
          testInput  = take minRep input

-- tries to match pattern with string
matchPattern :: [PatternToken] -> Input -> Bool
-- Repeaters should be matched first (they can match an empty string)
matchPattern (Repeater token minRep maxRep:rest) input = 
    any (matchPattern rest) $ repeatMatches token minRep maxRep input
matchPattern [] _ = True
matchPattern (AnchorEnd:_) [] = True
matchPattern (AnchorEnd:_) _ = False
matchPattern _ [] = False
matchPattern (Wildcard:rest) (_:input) = matchPattern rest input
matchPattern (Meta Digit:rest) (c:input)
    | isDigit c = matchPattern rest input
    | otherwise = False
matchPattern (Meta Word:rest) (c:input)
    | isWord c = matchPattern rest input
    | otherwise = False
matchPattern (Group group neg:rest) (c:input)
    | (c `elem` [x | Literal x <- group]) /= neg = matchPattern rest input
    | otherwise = False
matchPattern (Literal x:rest) (c:input)
    | (x == c) = matchPattern rest input
    | otherwise = False  
matchPattern (Repeater token minRep maxRep:rest) input = 
    any (matchPattern rest) $ repeatMatches token minRep maxRep input
matchPattern (Alteration _ alts:rest) input =
    any (True==) [matchPattern (alt ++ rest) input | alt <- alts] 


type State = [(Int, String)] -- associative list
-- pattern, input string, list of all possible branches and inputs for them
matchPatternExtra :: [PatternToken] -> Input -> [(State, Input)]
matchPatternExtra _ [] = []                     -- no matches, return "empty state"
matchPatternExtra [] input = [([], input)]
matchPatternExtra (AnchorEnd:_)  [] = [([], [])]
matchPatternExtra (AnchorEnd:_)  _  = [] 
matchPatternExtra (Alteration numGroup alts:rest) input =
    [(
        (numGroup, matchedString) : nextState, nextInput
     ) | alt <- alts,                                                           -- check all braches 
         (stateAlt, inputAlt) <- matchPatternExtra alt input,                   -- get input for the recursive call
         let matchedString  = take (length input - length matchedString) input, -- the only easy part
         (nextState, nextInput) <- matchPatternExtra rest inputAlt              -- recursive call
    ]
matchPatternExtra (Repeater token minRep maxRep:rest) input =
    [ 
    ]   
