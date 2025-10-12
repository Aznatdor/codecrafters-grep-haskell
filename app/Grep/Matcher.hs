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
    where pattern = (applyRepeat . parsePattern) rawPattern

-- matches repeated token and returns all possible input
-- tails after matching
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
-- Repeaters should be first to account for 0 times repetitions
matchPattern (Repeater token minRep maxRep:rest) input = 
    any (matchPattern rest) $ repeatMatches token minRep maxRep input
matchPattern [] _ = True
matchPattern (AnchorEnd:_) [] = True
matchPattern (AnchorEnd:_) _ = False
matchPattern _ [] = False
matchPattern (Wildcard:rest) (_:input) = matchPattern rest input
matchPattern (Meta Digit:rest) (c:input)
