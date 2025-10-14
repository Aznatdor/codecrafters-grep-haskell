module Grep.Matcher (mainMatch, matchPatternExtra) where

import Data.Char (isDigit)
import Data.List (tails, isPrefixOf)
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
-- TO-DO: add support for alterations, backreferences
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
matchPattern (Alteration _ alts:rest) input =
    any (True==) [matchPattern (alt ++ rest) input | alt <- alts] 

--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
-- Code to deal with patterns with backreferences
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
type Env = [(Int, String)]      -- (Environment) associative list. Maps group number to matched string
type State = [(Env, String)]    -- state of all branches. Current environment and string to match

-- Extended version of the mainMatch function
mainMatchExtra :: Pattern -> Input -> Bool
mainMatchExtra rawPattern input
    | (not . null) rawPattern && head rawPattern == '^' = any (null . snd) $ matchPatternExtra (tail pattern) [([], input)]
    | otherwise = any (null . snd) $ matchPatternExtra pattern $ zip (repeat []) (tails input)
    where pattern = (applyRepeat . parsePattern 1) rawPattern

repeatMatchesExtra :: PatternToken -> Int -> Maybe Int -> Env -> String -> State
repeatMatchesExtra _ _ _ _ [] = []
-- Calling matchPatternExtra would yield State variable. We can't omit Env variables (e.g. "(a)+\\1" - we match some group within repetition)
repeatMatchesExtra token minRep Nothing env input =
     let state = matchPatternExtra testPattern [(env, input)] in -- to prevent infinite loops
        case state of
        [] -> []
        _  -> state ++ (repeatMatchesExtra token (minRep+1) Nothing env input)
    where testPattern = replicate minRep token
repeatMatchesExtra token minRep (Just maxRep) env input 
    | minRep <= maxRep = matchPatternExtra testPattern [(env, input)] ++ (repeatMatchesExtra token (minRep+1) (Just maxRep) env input)
    | otherwise = []
    where testPattern = replicate minRep token

-- pattern, current state, list of all possible branches and inputs for them
matchPatternExtra :: [PatternToken] -> State -> State
-- Base case. Just return the accumulated state
matchPatternExtra [] state = state
-- For token types AnchorEnd, Literal, Meta, Group, Backreference just filter state and propagate it.
matchPatternExtra (AnchorEnd:_) state  = filter (null . snd) state
matchPatternExtra (Literal x:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, drop 1 input) | 
            (env, input) <- state,
            let recInput = tail input,
            not (null input), x == (head input)
            ]
matchPatternExtra (Meta Digit:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, drop 1 input) | 
            (env, input) <- state,
            let nextInput = tail input,
            not (null input), (isDigit . head) input
            ]
matchPatternExtra (Meta Word:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, drop 1 input) | 
            (env, input) <- state,
            let nextInput = tail input,
            not (null input), (isWord . head) input 
            ]
matchPatternExtra (Group group neg:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, drop 1 input) | 
            (env, input) <- state,
            let nextInput = tail input,
            not (null input), (inGroup . head) input 
            ]
          inGroup c = (c `elem` [x | Literal x <- group]) /= neg
matchPatternExtra (Wildcard:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, drop 1 input) | (env, input) <- state, (not . null) input]
matchPatternExtra (Backreference groupNum:rest) state = matchPatternExtra rest nextState
    where nextState = [(env, nextInput) |
            (env, input) <- state,
            Just ref <- [lookup groupNum env],                                -- might return Nothing. Insterested only in Just values
            let nextInput = drop (length ref) input,
            ref `isPrefixOf` input
            ]
-- Add new inputs in state and go further
matchPatternExtra (Repeater token minRep maxRep:rest) state = matchPatternExtra rest nextState
    where nextState = foldl (++) [] [repeatMatchesExtra token minRep maxRep env input | (env, input) <- state]
-- In case of Alternation, return final result. Note recursive calls within list comprehension
matchPatternExtra (Alteration groupNum alts:rest) state =
    [(newEnv, newInput) |
        (env, input) <- state,                                              -- check all inputs
        alt <- alts,                                                        -- check all alternatives
        (_, nextInput) <- matchPatternExtra alt state,                      -- input for recursive call
        let matchedString = take (length input - length nextInput) input,   -- string to update current environment
        let currentMatch = (groupNum, matchedString),
        let nextEnv = currentMatch : env,
        let nextState = [(nextEnv, nextInput)],                             -- state for recursive call
        (newEnv, newInput) <- matchPatternExtra rest nextState
    ]
