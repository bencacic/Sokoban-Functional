{- |
Module      :  SokobanInput
Description :  Handles the input and puzzle reading from a file.

Creators  :  bcaci729@mtroyal.ca, kcaro419@mtroyal.ca, marab065@mtroyal.ca

-}
module SokobanInput
    ( readSokobanFromFile
    ) where

import System.Environment
import SokobanSolver
import Control.Exception
import SokobanDataTypes

-- Read puzzle from file that contains a puzzle.
readSokobanFromFile :: FilePath -> IO (Maybe SokobanPuzzle)
readSokobanFromFile filePath = do
    contents <- readFile filePath
    let puzzleLines = lines contents
    return (parseSokoban puzzleLines)

-- Parses a puzzle state from a list that contains lines from the input file
parseSokoban :: [String] -> Maybe SokobanPuzzle
parseSokoban puzzleLines = do
    let rows = length puzzleLines
        cols = maximum (map length puzzleLines)
        gameState = mapM (parseRow cols) puzzleLines
    case gameState of
        Right gs -> if rows > 0 && cols > 0 && length (filter (== Player) (concat gs)) <= 1
                        then Just (SokobanPuzzle gs)
                        else Nothing
        Left err -> Nothing

-- Converts each character from a line into a tile type
parseRow :: Int -> String -> Either String [TileType]
parseRow cols line =
    let paddedLine = line ++ replicate (cols - length line) ' '
    in sequence (map tileTypeFromChar paddedLine)

-- Stores a character from the input file into a tile type
tileTypeFromChar :: Char -> Either String TileType
tileTypeFromChar 'X' = Right Wall
tileTypeFromChar ' ' = Right Empty
tileTypeFromChar 'P' = Right Player
tileTypeFromChar 'O' = Right PlayerGoal
tileTypeFromChar 'B' = Right Box
tileTypeFromChar 'H' = Right BoxGoal
tileTypeFromChar 'G' = Right Goal
tileTypeFromChar _   = Left "Invalid character in Sokoban puzzle"