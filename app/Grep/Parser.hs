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
parsePattern (')':xs) = error $ "unexpected ')'"
parsePattern ('(':xs) =
    let (alterationGroup, rest) = parseAlteration xs [[]]
    in Alteration alterationGroup : parsePattern rest
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


-- Helper function to parse group of alteration, e.g. (ab|c|de)
parseAlteration :: String -> [[PatternToken]]-> ([[PatternToken]], String)
-- End of the group. Return the result
parseAlteration (')':rest) acc = (acc, rest)
-- Inner group of alterations
-- For this case we should also know what is going after the group
parseAlteration ('(':rest) acc = parseAlteration after updatedAcc
    where (inner, after) = parseAlteration rest [[]]
          innerAlteration = Alteration inner
          newHead = (head acc ++ [innerAlteration])
          updatedAcc = newHead : (drop 1 acc)
-- Add new alteration
parseAlteration ('|':rest) acc = parseAlteration rest ([] : acc)
-- Regular case: just append current token to the first list
parseAlteration ('\\':'w':rest) acc = parseAlteration rest updatedAcc
    where token = Meta Word
          newHead = (head acc ++ [token]) -- slower than ':'. But without reversal
          updatedAcc = newHead : (drop 1 acc) -- tail won't work with list of length 1
parseAlteration ('\\':'d':rest) acc = parseAlteration rest updatedAcc
    where token = Meta Digit
          newHead = (head acc ++ [token])
          updatedAcc = newHead : (drop 1 acc)
parseAlteration (c:rest) acc = parseAlteration rest updatedAcc
    where token = Literal c
          newHead = (head acc) ++ [token]
          updatedAcc = newHead : (drop 1 acc)
