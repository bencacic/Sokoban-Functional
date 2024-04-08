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

-- parseSokoban :: [String] -> Maybe SokobanPuzzle
-- parseSokoban puzzleLines = do
--     let rows = length puzzleLines
--         cols = maximum (map length puzzleLines)
--         gameState = mapM (parseRow cols) puzzleLines
--     case gameState of
--         Right gs -> if rows > 0 && cols > 0 && playerCount gs <= 1
--                         then Just (SokobanPuzzle gs)
--                         else Nothing
--         Left err -> Nothing
--   where
--     playerCount gs = length $ filter (\x -> x == Player || x == PlayerGoal) (concat gs)
parseSokoban :: [String] -> Maybe SokobanPuzzle
parseSokoban puzzleLines = do
    let rows = length puzzleLines
        cols = maximum (map length puzzleLines)
        gameState = mapM (parseRow cols) puzzleLines
    case gameState of
        Right gs -> if playerCount gs == 0 && numGoals gs > 0 
                        then Nothing
                    else if numGoals gs > numBoxes gs 
                        then Nothing
                    else if rows > 0 && cols > 0 && playerCount gs <= 1
                        then Just (SokobanPuzzle gs)
                        else Nothing
        Left err -> Nothing
  where
    playerCount gs = length $ filter (\x -> x == Player || x == PlayerGoal) (concat gs)
    numBoxes    gs = length  (filter (\x -> x == Box) (concat gs))
    numGoals    gs = length  (filter (\x -> x == Goal) (concat gs))

parseRow :: Int -> String -> Either String [TileType]
parseRow cols line =
    let paddedLine = line ++ replicate (cols - length line) ' '
    in sequence (map tileTypeFromChar paddedLine)

tileTypeFromChar :: Char -> Either String TileType
tileTypeFromChar 'X' = Right Wall
tileTypeFromChar ' ' = Right Empty
tileTypeFromChar 'P' = Right Player
tileTypeFromChar 'O' = Right PlayerGoal
tileTypeFromChar 'B' = Right Box
tileTypeFromChar 'H' = Right BoxGoal
tileTypeFromChar 'G' = Right Goal
tileTypeFromChar _   = Left "Invalid character in Sokoban puzzle"