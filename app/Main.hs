module Main where

import System.Environment
import System.Exit
import System.IO (hPutStrLn, hSetBuffering, stdout, stderr, BufferMode (NoBuffering))

import Grep.Handle

main :: IO ()
main = do
    -- Disable output buffering
    hSetBuffering stdout NoBuffering
    hSetBuffering stderr NoBuffering

    args <- getArgs
    case args of
        ("-E":pattern:[]) -> handleLine pattern
        ("-E":pattern:fileName:[]) -> handleFile pattern fileName
        ("-E":pattern:fileNames) -> handleFiles pattern fileNames
        ("-r":"-E":pattern:dirName:[]) -> handleDirRec pattern dirName
        _ -> do
            putStrLn "Invalid arguments.\nUsage:\n  prog -E <pattern> [file1|file2|...]\n  prog -r -E <pattern> <dir>"
            exitFailure
