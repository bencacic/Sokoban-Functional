module SokobanInput
    ( readSokobanFromFile
    ) where

import System.Environment
import SokobanSolver
import Control.Exception
import SokobanDataTypes

readSokobanFromFile :: FilePath -> IO (Maybe SokobanPuzzle)
readSokobanFromFile filePath = do
    contents <- readFile filePath
    let puzzleLines = lines contents
    return (parseSokoban puzzleLines)

parseSokoban :: [String] -> Maybe SokobanPuzzle
parseSokoban puzzleLines = do
    let rows = length puzzleLines
        cols = maximum (map length puzzleLines)
        gameState = mapM (parseRow cols) puzzleLines
    case gameState of
        Prelude.Right gs -> if rows > 0 && cols > 0 && length (filter (== Player) (concat gs)) == 1
                        then Just (SokobanPuzzle gs)
                        else Nothing
        Prelude.Left err -> Nothing

parseRow :: Int -> String -> Either String [TileType]
parseRow cols line =
    let paddedLine = line ++ replicate (cols - length line) ' '
    in sequence (map tileTypeFromChar paddedLine)

tileTypeFromChar :: Char -> Either String TileType
tileTypeFromChar 'X' = Prelude.Right Wall
tileTypeFromChar ' ' = Prelude.Right Empty
tileTypeFromChar 'P' = Prelude.Right Player
tileTypeFromChar 'O' = Prelude.Right PlayerGoal
tileTypeFromChar 'B' = Prelude.Right Box
tileTypeFromChar 'H' = Prelude.Right BoxGoal
tileTypeFromChar 'G' = Prelude.Right Goal
tileTypeFromChar _   = Prelude.Left "Invalid character in Sokoban puzzle"