module Main where

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
        ("-E":pattern:fileNames) -> handleFiles pattern fileName
        ("-r":"-E":pattern:dirName) -> handleDirRec pattern dirName
        _ -> do
            putStrLn "Invalid arguments" 
            exitFailure
