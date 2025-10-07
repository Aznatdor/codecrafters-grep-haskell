module Grep.Parser where

import Grep.Types

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


applyRepeat :: [PatternToken] -> [PatternToken]
applyRepeat [] = []
applyRepeat (token:Plus:rest) = Repeater token 1 Nothing : applyRepeat rest
applyRepeat (token:Star:rest) = Repeater token 0 Nothing : applyRepeat rest
applyRepeat (token:Question:rest) = Repeater token 0 (Just 1) : applyRepeat rest
applyRepeat (token:rest) = token : applyRepeat rest
