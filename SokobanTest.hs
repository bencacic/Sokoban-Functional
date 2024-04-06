module SokobanTest where

import SokobanInput (readSokobanFromFile)
import SokobanSolver (solvePuzzle)
import System.Directory
import System.FilePath
import System.Environment (getArgs)
import Control.Monad (mapM_)
import Data.List (sort)

runTests :: IO ()
runTests = do
    let folderPath = "testPuzzles"
    files <- listDirectory folderPath
    let sortedFiles = sort files
    mapM_ (\file -> testPuzzle (folderPath </> file)) sortedFiles

testPuzzle :: FilePath -> IO ()
testPuzzle filePath = do
    putStrLn $ "Testing puzzle: " ++ filePath
    puzzle <- readSokobanFromFile filePath
    case puzzle of
        Just sokoban -> do
            case solvePuzzle sokoban of
                Just steps -> do
                    putStrLn $ "Puzzle solved: " ++ takeFileName filePath
                Nothing ->
                    putStrLn $ "Puzzle unsolvable: " ++ takeFileName filePath
        Nothing ->
            putStrLn $ "Failed to read puzzle from file: " ++ takeFileName filePath