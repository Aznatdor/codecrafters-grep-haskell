module Main where

import ParseGroup
import System.Environment
import System.Exit
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import Data.Char (isDigit)

isWord :: Char -> Bool
isWord c = c `elem` '_':['a'..'z']++['A'..'Z']++['0'..'9']

type Input = String
type Pattern = String


inGroup :: String -> Char -> Bool
inGroup group c = c `elem` group


mainMatch :: Pattern -> Input -> Bool
mainMatch ('^':pattern) input = matchPattern pattern input
mainMatch pattern input = case last pattern of 
                        '$' -> any (matchPattern $ (init pattern) ++ "\0")) $ unfold (input ++ "\0") -- add special char to the end
                        _  -> any (matchPattern pattern) $ unfold input


matchPattern :: Pattern -> Input -> Bool
matchPattern [] _ = True
matchPattern _ [] = False
matchPattern ('\\':'d':pattern) (c:input) = (isDigit c) && matchPattern pattern input
matchPattern ('\\':'w':pattern) (c:input) = (isWord c) && matchPattern pattern input
matchPattern ('[':'^':group) (c:input)    = case find ']' group of 
                                                Left (-1) -> error "Could not find closing ] bracket" 
                                                Right ind -> (not . elem c) (getGroup group) && matchPattern (drop (ind+1) group) input
matchPattern ('[':group) (c:input) = case find ']' group of 
                                                Left (-1) -> error "Could not find closing ] bracket"
                                                Right ind -> elem c (getGroup group) && matchPattern (drop (ind+1) group) input
matchPattern (x:pattern) (c:input) = (x == c) && matchPattern pattern input
matchPattern pattern _ =  error $ "Unhandled pattern: " ++ pattern

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
