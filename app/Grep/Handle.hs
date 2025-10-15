module Grep.Handle (handleLine, handleFile, handleFiles, handleDirRec) where 

import System.Environment
import System.Exit
import System.Directory (listDirectory, doesDirectoryExist, doesFileExist)
import System.FilePath ((</>))
import Control.Monad (forM)
import System.IO 

import Grep.Matcher -- core matching functionality

-- Process input line
handleLine :: String -> IO ()
handleLine pattern = do
    inputLine <- getLine
    if mainMatchExtra pattern inputLine
        then exitSuccess
        else exitFailure

-- Process single file. Return action
handleFile :: String -> FilePath -> IO ()
handleFile pattern path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let matchedLines = filter (mainMatchExtra pattern) (lines contents)
    if (not . null) matchedLines
        then do 
            mapM_ putStrLn matchedLines
            exitSuccess
        else exitFailure
    hClose handle

-- Process one file of list of files. Return IO [String]
handleFilesHelp :: String -> FilePath -> IO [String]
handleFilesHelp pattern path = do
    handle <- openFile path ReadMode
    contents <- hGetContents handle
    let matchedLines = filter (mainMatchExtra pattern) (lines contents)
    return $ map (\line -> path ++ ":" ++ line) matchedLines

-- Process list of files
handleFiles :: String -> [FilePath] -> IO ()
handleFiles pattern fileNames = do
    results <- mapM (handleFilesHelp pattern) fileNames
    let finalStr = concat results
    if (not . null) finalStr
        then do
            mapM_ putStrLn finalStr
            exitSuccess
        else exitFailure

listFilesRecursive :: FilePath -> IO [FilePath]
listFilesRecursive path = do
  entries <- listDirectory path             -- get contents of the directory
  paths <- forM entries $ \name -> do       
    let fullPath = path </> name            -- get full path
    isDir <- doesDirectoryExist fullPath
    if isDir
      then listFilesRecursive fullPath      -- if dir, apply this function
      else pure [fullPath]                  -- else return "just" the name of the file
  pure (concat paths)

-- Traverses directory recursively
handleDirRec :: String -> FilePath -> IO ()
handleDirRec pattern dirName = do
    files <- listFilesRecursive dirName
    handleFiles pattern files

main = do
    let path = "testDir"
    handleDirRec "a+" path
