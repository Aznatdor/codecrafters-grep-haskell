module Grep.Parser where

import Grep.Types

-- Given pattern string, converts it into list of tokens
-- Token types are listed in app/Grep/Types.hs
parsePattern :: String -> Int -> [PatternToken]
parsePattern [] _ = []
parsePattern ('\\':'d':rest) groupNum = Meta Digit : (parsePattern rest groupNum)
parsePattern ('\\':'w':rest) groupNum = Meta Word  : (parsePattern rest groupNum)
parsePattern ('[':'^':xs)    groupNum =
    let (group, rest) = span (/=']') xs
    in Group (map Literal group) True : (parsePattern (drop 1 rest) groupNum)
parsePattern ('[':xs) groupNum = let (group, rest) = span (/=']') xs in
                            Group (map Literal group) False : (parsePattern (drop 1 rest) groupNum)
parsePattern (')':xs) _ = error $ "unexpected ')'"
parsePattern ('(':xs) groupNum =
    let (alterationGroup, rest, newGroupNum) = parseAlteration xs [[]] (groupNum+1) -- newGroupNum should be at lest groupNum+1. Indeed!
    in Alteration groupNum alterationGroup : (parsePattern rest newGroupNum) -- group is reversed, which should not affect overall result.
parsePattern ('^':rest) groupNum = AnchorStart : (parsePattern rest groupNum)
parsePattern ('$':rest) groupNum = AnchorEnd : (parsePattern rest groupNum) -- rest is []!
parsePattern ('+':rest) groupNum = Plus : (parsePattern rest groupNum)
parsePattern ('*':rest) groupNum = Star : (parsePattern rest groupNum)
parsePattern ('?':rest) groupNum = Question : (parsePattern rest groupNum)
parsePattern ('.':rest) groupNum = Wildcard : (parsePattern rest groupNum)
parsePattern (char:rest) groupNum = Literal char : (parsePattern rest groupNum)

-- Joins repeater token and token to be repeated
applyRepeat :: [PatternToken] -> [PatternToken]
applyRepeat [] = []
applyRepeat (token:Plus:rest) = Repeater token 1 Nothing : applyRepeat rest
applyRepeat (token:Star:rest) = Repeater token 0 Nothing : applyRepeat rest
applyRepeat (token:Question:rest) = Repeater token 0 (Just 1) : applyRepeat rest
applyRepeat (token:rest) = token : applyRepeat rest


-- Helper function to parse group of alteration, e.g. (ab|c|de)
-- returns content of group, rest of the string to parse, next group number
parseAlteration :: String -> [[PatternToken]] -> Int -> ([[PatternToken]], String, Int)
-- End of the group. Return the result
parseAlteration (')':rest) acc groupNum = (acc, rest, groupNum)
-- Inner group of alterations
-- For this case we should also know what is going after the group
parseAlteration ('(':rest) acc groupNum = parseAlteration after updatedAcc newGroupNum
    where (inner, after, newGroupNum) = parseAlteration rest [[]] (groupNum+1)
          innerAlteration = Alteration groupNum inner
          newHead = (head acc ++ [innerAlteration])
          updatedAcc = newHead : (drop 1 acc)
-- Add new alteration
parseAlteration ('|':rest) acc groupNum = parseAlteration rest ([] : acc) groupNum
-- Regular case: just append current token to the first list
parseAlteration ('\\':'w':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Meta Word
          newHead = (head acc ++ [token]) -- slower than ':'. But without reversal
          updatedAcc = newHead : (drop 1 acc) -- tail won't work with list of length 1
parseAlteration ('\\':'d':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Meta Digit
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration (c:rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Literal c
          newHead = (head acc) ++ [token]
          updatedAcc = newHead : (drop 1 acc)
