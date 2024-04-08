{- |
Module      :  SokobanTest
Description :  The test driver for our solver.

Creators  :  bcaci729@mtroyal.ca, kcaro419@mtroyal.ca, marab065@mtroyal.ca

-}
module SokobanTest where

import SokobanInput (readSokobanFromFile)
import SokobanSolver (solvePuzzle)
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import Control.Monad (mapM_)
import Data.List (sort)
import Control.Exception (try, SomeException)

-- Runs the tests stored in the "testPuzzles" directory
runTests :: IO ()
runTests = do
    let folderPath = "testPuzzles"
    files <- listDirectory folderPath
    let sortedFiles = sort files
    mapM_ (\file -> testPuzzle (folderPath </> file)) sortedFiles

-- Determines whether a particular test passes or fails (or cannot be read)
testPuzzle :: FilePath -> IO ()
testPuzzle filePath = do
    result <- try $ do
        puzzle <- readSokobanFromFile filePath
        case puzzle of
            Just sokoban -> do
                case solvePuzzle sokoban of
                    Just _ -> putStrLn $ "Puzzle solved: " ++ takeFileName filePath ++ "\n"
                    Nothing -> putStrLn $ "Puzzle unsolvable: " ++ takeFileName filePath ++ "\n"
            Nothing -> putStrLn $ "Failed to read puzzle from file: " ++ takeFileName filePath ++ "\n"
    case result of
        Left (e :: SomeException) -> putStrLn $ "Error occurred: " ++ show e ++ "\n"
        _ -> return ()