module Grep.Parser where

import Data.Char (isDigit, digitToInt)
import Grep.Types 

-- Given pattern string, converts it into list of tokens
-- Token types are listed in app/Grep/Types.hs
parsePattern :: Int -> String -> [PatternToken]
parsePattern _ [] = []
parsePattern groupNum ('\\':x:rest)
    | x == 'd'  = Meta Digit : (parsePattern groupNum rest)
    | x == 'w'  = Meta Word  : (parsePattern groupNum rest)
    | x == '\\' = Literal '\\' : (parsePattern groupNum rest)
    | isDigit x = Backreference (digitToInt x) : (parsePattern groupNum rest)
parsePattern groupNum ('[':'^':xs) =
    let (group, rest) = span (/=']') xs
    in Group (map Literal group) True : (parsePattern groupNum (drop 1 rest))
parsePattern groupNum ('[':xs) = let (group, rest) = span (/=']') xs in
                            Group (map Literal group) False : (parsePattern groupNum (drop 1 rest))
parsePattern  _ (')':xs) = error $ "unexpected ')'"
parsePattern groupNum ('(':xs) =
    let (alterationGroup, rest, newGroupNum) = parseAlteration xs [[]] (groupNum+1) -- newGroupNum should be at least groupNum+1.
    in Alteration groupNum (map applyRepeat alterationGroup) : (parsePattern newGroupNum rest) -- group is reversed, which should not affect overall result.
parsePattern groupNum ('^':rest) = AnchorStart : (parsePattern groupNum rest)
parsePattern groupNum ('$':rest) = AnchorEnd : (parsePattern groupNum rest) -- rest is []!
parsePattern groupNum ('+':rest) = Plus : (parsePattern groupNum rest)
parsePattern groupNum ('*':rest) = Star : (parsePattern groupNum rest)
parsePattern groupNum ('?':rest) = Question : (parsePattern groupNum rest)
parsePattern groupNum ('.':rest) = Wildcard : (parsePattern groupNum rest)
parsePattern groupNum ('{':xs) = Quantifier minRep maxRep : (parsePattern groupNum $ drop 1 rest)
    where (group, rest) = span (/='}') xs
          (minRepStr, maxRepStrRaw) = span (/=',') group
          maxRepStr = drop 1 maxRepStrRaw
          minRep :: Int
          minRep = read minRepStr
          maxRep :: Maybe Int
          maxRep = if null maxRepStr then Nothing else Just $ read maxRepStr
parsePattern groupNum (char:rest) = Literal char : (parsePattern groupNum rest)

-- Joins repeater token and token to be repeated
applyRepeat :: [PatternToken] -> [PatternToken]
applyRepeat [] = []
applyRepeat (token:Plus:rest) = Repeater token 1 Nothing : applyRepeat rest
applyRepeat (token:Star:rest) = Repeater token 0 Nothing : applyRepeat rest
applyRepeat (token:Question:rest) = Repeater token 0 (Just 1) : applyRepeat rest
applyRepeat (token:Quantifier minRep maxRep:rest) = Repeater token minRep maxRep : applyRepeat rest
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
          innerAlteration = Alteration groupNum $ map applyRepeat inner -- don't forget to apply repeaters for insides of a group!!!
          newHead = (head acc ++ [innerAlteration])
          updatedAcc = newHead : (drop 1 acc)
-- Add new alteration
parseAlteration ('|':rest) acc groupNum = parseAlteration rest ([] : acc) groupNum
-- Regular case: just append current token to the first list
parseAlteration ('[':'^':xs) acc groupNum = parseAlteration (drop 1 rest) updatedAcc groupNum
    where (group, rest) = span (/=']') xs
          token = Group (map Literal group) True
          newHead = head acc ++ [token]
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('[':xs) acc groupNum = parseAlteration (drop 1 rest) updatedAcc groupNum
    where (group, rest) = span (/=']') xs
          token = Group (map Literal group) False
          newHead = head acc ++ [token]
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('.':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Wildcard
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('\\':'w':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Meta Word
          newHead = (head acc ++ [token]) -- slower than ':'. But without reversal
          updatedAcc = newHead : (drop 1 acc) -- tail won't work with list of length 1
parseAlteration ('\\':'d':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Meta Digit
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('\\':digit:rest) acc groupNum = parseAlteration rest updatedAcc groupNum -- <--- might fall here!!!! No check if digit is really a digit!!!!
    where token = Backreference (digitToInt digit)
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('+':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Plus
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('?':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Question
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration ('*':rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Star
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration (c:rest) acc groupNum = parseAlteration rest updatedAcc groupNum
    where token = Literal c
          newHead = (head acc) ++ [token]
          updatedAcc = newHead : (drop 1 acc)
