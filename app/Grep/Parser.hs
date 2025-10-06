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
parsePattern (char:rest) = Literal char : parsePattern rest
