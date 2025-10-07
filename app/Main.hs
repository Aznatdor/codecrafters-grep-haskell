module Main where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import Data.Char (isDigit, isLetter)
import Data.List (findIndex, tails)
import Grep.Types
import Grep.Parser

-- useful synonims
type Input = String
type Pattern = String

-- helper functions
getGroup :: String -> String
getGroup pattern = takeWhile (/=']') pattern

isWord :: Char -> Bool
isWord c = c `elem` '_':['a'..'z']++['A'..'Z']++['0'..'9']


inGroup :: String -> Char -> Bool
inGroup group c = c `elem` group


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
    | matchPattern (replicate minRep token) (take minRep input) = drop minRep input : repeatMatches token (minRep+1) Nothing input
    | otherwise = [] -- if no match, return input string
repeatMatches token minRep (Just maxRep) input
    | minRep <= maxRep && matchPattern (replicate minRep token) (take minRep input) = drop minRep input : repeatMatches token (minRep+1) (Just maxRep) input
    | otherwise = []


-- matches pattern with string
matchPattern :: [PatternToken] -> Input -> Bool
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


main :: IO ()
main = do
  -- Disable output buffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr NoBuffering

  args <- getArgs
  let pattern = args !! 1
  input_line <- getLine

  -- You can use print statements as follows for debugging, they'll be visible when running tests.
  hPutStrLn stderr "Logs from your program will appear here"

  if head args /= "-E"
    then do
      putStrLn "Expected first argument to be '-E'"
      exitFailure
    else do if mainMatch pattern input_line
              then exitSuccess
              else exitFailure
