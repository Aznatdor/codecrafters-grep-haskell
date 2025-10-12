module Grep.Parser where

import Grep.Types

-- Given pattern string, converts it into list of tokens
-- Token types are listed in app/Grep/Types.hs
parsePattern :: String -> [PatternToken]
parsePattern [] = []
parsePattern ('\\':'d':rest) = Meta Digit : parsePattern rest
parsePattern ('\\':'w':rest) = Meta Word  : parsePattern rest
parsePattern ('[':'^':xs)  = let (group, rest) = span (/=']') xs in
                                Group (map Literal group) True : parsePattern (drop 1 rest)
parsePattern ('[':xs) = let (group, rest) = span (/=']') xs in
                            Group (map Literal group) False : parsePattern (drop 1 rest)
parsePattern ('^':rest) = AnchorStart : parsePattern rest
parsePattern ('$':rest) = AnchorEnd : parsePattern rest -- rest is []!
parsePattern ('+':rest) = Plus : parsePattern rest
parsePattern ('*':rest) = Star : parsePattern rest
parsePattern ('?':rest) = Question : parsePattern rest
parsePattern ('.':rest) = Wildcard : parsePattern rest
parsePattern (char:rest) = Literal char : parsePattern rest

-- Joins repeater token and token to be repeated
applyRepeat :: [PatternToken] -> [PatternToken]
applyRepeat [] = []
applyRepeat (token:Plus:rest) = Repeater token 1 Nothing : applyRepeat rest
applyRepeat (token:Star:rest) = Repeater token 0 Nothing : applyRepeat rest
applyRepeat (token:Question:rest) = Repeater token 0 (Just 1) : applyRepeat rest
applyRepeat (token:rest) = token : applyRepeat rest


-- Function to parse group of alterations (..)
-- parseAlteration :: Int -> String -> ([PatternToken], String)
-- parseAlteration groupNum (')':rest) = ([], rest) -- end of the group
-- -- Group inside the current group
-- parseAlteration groupNum ('(':rest) =
--     let 
-- parseAlteration groupNum ('\\':'d':rest) = 
--     let (group, after) = parseAlteration groupNum rest
--     in (Meta Digit:group, after)
-- parseAlteration groupNum ('\\':'w':rest) = 
--     let (group, after) = parseAlteration groupNum rest
--     in (Meta Word:group, after)
-- parseAlteration groupNum (c:rest) =
--     let (group, after) = parseAlteration groupNum rest
--     in (Literal cd:group, after)


parseAlteration :: String -> [[PatternToken]]-> ([[PatternToken]], String)
-- End of the group. Return the result
parseAlteration (')':rest) acc = (acc, rest)
-- Inner group of alterations
-- For this case we should also know what is going after the group
parseAlteration ('(':rest) acc =
    let (newAcc, rest1) = parseAlteration after updatedAcc
    in (newAcc, rest1)
    where (inner, after) = parseAlteration rest [[]]
          innerAlteration = Alteration inner
          newHead = (head acc ++ [innerAlteration])
          updatedAcc = newHead : acc
-- Add new alteration
parseAlteration ('|':rest) acc =
    let (newAcc, after) = parseAlteration rest ([] : acc)
    in (newAcc, after)
-- Regular case: just append current token to the first list
parseAlteration ('\\':'w':rest) acc =
    let (newAcc, after) = parseAlteration rest updatedAcc
    in (newAcc, after)
    where token = Meta Word
          newHead = (head acc ++ [token]) -- slower than ':'. But without reversal
          updatedAcc = newHead : (drop 1 acc) -- tail won't work with list of length 1
parseAlteration ('\\':'d':rest) acc =
    let (newAcc, after) = parseAlteration rest updatedAcc
    in (newAcc, after)
    where token = Meta Digit
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration (c:rest) acc = 
    let (newAcc, after) = parseAlteration rest updatedAcc
    in (newAcc, after)
    where token = Literal c
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
