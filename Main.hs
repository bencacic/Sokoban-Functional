import SokobanDataTypes
import SokobanSolver
import SokobanInput
import Data.Maybe (fromJust)

-- Function to print the puzzle solution
printSolution :: [SokobanPuzzle] -> IO ()
printSolution solutionStack = do
    putStrLn "Solution found:"
    printDirections solutionStack
    putStrLn "Puzzle solved."

-- Function to print the directions between puzzle states
printDirections :: [SokobanPuzzle] -> IO ()
printDirections [] = return ()
printDirections [_] = return ()
printDirections (current:prev:rest) = do
    putStrLn (showDirection current prev)
    printDirections (prev:rest)

-- Function to determine the direction of movement between two puzzle states
showDirection :: SokobanPuzzle -> SokobanPuzzle -> String
showDirection current prev = 
    case (playerRow current, playerCol current, playerRow prev, playerCol prev) of
        (rowCurr, colCurr, rowPrev, colPrev)
            | rowCurr == rowPrev && colCurr < colPrev -> "Left"
            | rowCurr == rowPrev && colCurr > colPrev -> "Right"
            | rowCurr < rowPrev && colCurr == colPrev -> "Up"
            | rowCurr > rowPrev && colCurr == colPrev -> "Down"
            | otherwise -> "Unknown"
    where 
        playerRow (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
                                Just index -> index `div` length (head gameState)
                                Nothing -> -1
        playerCol (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
                                Just index -> index `mod` length (head gameState)
                                Nothing -> -1
                                
-- Main function
main :: IO ()
main = do
    putStrLn "Testing readSokobanFromFile..."
    puzzle <- readSokobanFromFile
    case puzzle of
        Just sokoban -> do
            putStrLn "Puzzle successfully read:"
            case solvePuzzle sokoban of
                Just steps -> do
                    putStrLn "Moves taken to solve the puzzle:"
                    printSolution steps
                Nothing ->
                    putStrLn "No solution found for this puzzle."
        Nothing -> putStrLn "Failed to read puzzle from file."