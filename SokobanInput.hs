module SokobanFileInput
    ( readSokobanFromFile
    ) where

import System.Environment
import SokobanDataTypes
import SokobanSolver
import Control.Exception

-- Function to read a Sokoban puzzle from a file
readSokobanFromFile :: IO (Maybe SokobanPuzzle)
readSokobanFromFile = do
    args <- getArgs
    case args of
        [filePath] -> do
            contents <- readFile filePath
            let puzzleLines = lines contents
            return (parseSokoban puzzleLines)
        _ -> do
            putStrLn "Usage: sokoban-solver <input-file>"
            return Nothing

-- Function to parse Sokoban puzzle from lines of text
parseSokoban :: [String] -> Maybe SokobanPuzzle
parseSokoban puzzleLines =
    if rows > 0 && cols > 0
        then Just (SokobanPuzzle gameState)
        else Nothing
    where
        rows = length puzzleLines
        cols = maximum (map length puzzleLines)
        gameState = map (parseRow cols) puzzleLines

-- Function to parse a row of the puzzle
parseRow :: Int -> String -> [TileType]
parseRow cols line =
    let paddedLine = line ++ replicate (cols - length line) ' '
    in map tileTypeFromChar paddedLine

-- Convert character to TileType
tileTypeFromChar :: Char -> TileType
tileTypeFromChar 'X' = Wall
tileTypeFromChar ' ' = Empty
tileTypeFromChar 'P' = Player
tileTypeFromChar 'O' = PlayerGoal
tileTypeFromChar 'B' = Box
tileTypeFromChar 'H' = BoxGoal
tileTypeFromChar 'G' = Goal
tileTypeFromChar _ = error "Invalid character in Sokoban puzzle"