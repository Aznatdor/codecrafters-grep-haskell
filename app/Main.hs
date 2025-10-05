module Main where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode (NoBuffering))
import Data.Char (isDigit)

isWord :: Char -> Bool
isWord c = c `elem` '_':['a'..'z']++['A'..'Z']++['0'..'9']

matchPattern :: String -> String -> Bool
matchPattern "\\d" input = any isDigit input
matchPattern "\\w" input = any isWord input
matchPattern (x:_) input = x `elem` input
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
    else do if matchPattern pattern input_line
              then exitSuccess
              else exitFailure
