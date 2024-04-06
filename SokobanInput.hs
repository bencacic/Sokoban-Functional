module SokobanInput
    ( readSokobanFromFile
    ) where

import System.Environment
import SokobanDataTypes
import SokobanSolver
import Control.Exception

readSokobanFromFile :: FilePath -> IO (Maybe SokobanPuzzle)
readSokobanFromFile filePath = do
    contents <- readFile filePath
    let puzzleLines = lines contents
    return (parseSokoban puzzleLines)

parseSokoban :: [String] -> Maybe SokobanPuzzle
parseSokoban puzzleLines =
    if rows > 0 && cols > 0
        then Just (SokobanPuzzle gameState)
        else Nothing
    where
        rows = length puzzleLines
        cols = maximum (map length puzzleLines)
        gameState = map (parseRow cols) puzzleLines

parseRow :: Int -> String -> [TileType]
parseRow cols line =
    let paddedLine = line ++ replicate (cols - length line) ' '
    in map tileTypeFromChar paddedLine

tileTypeFromChar :: Char -> TileType
tileTypeFromChar 'X' = Wall
tileTypeFromChar ' ' = Empty
tileTypeFromChar 'P' = Player
tileTypeFromChar 'O' = PlayerGoal
tileTypeFromChar 'B' = Box
tileTypeFromChar 'H' = BoxGoal
tileTypeFromChar 'G' = Goal
tileTypeFromChar _ = error "Invalid character in Sokoban puzzle"