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
    where pattern = parsePattern rawPattern


-- matches pattern with string
matchPattern :: [PatternToken] -> Input -> Bool
matchPattern [] _ = True
matchPattern (AnchorEnd:_) [] = True
matchPattern (AnchorEnd:_) _ = False
matchPattern _ [] = False
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
