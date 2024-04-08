{- |
Module      :  Main
Description :  Contains the main for the sokoban solver as well as output.

Creators  :  bcaci729@mtroyal.ca, kcaro419@mtroyal.ca, marab065@mtroyal.ca

-}
import SokobanDataTypes
import SokobanSolver
import SokobanInput
import SokobanTest(runTests)
import System.Environment (getArgs)

-- Prints the puzzle solution
printSolution :: [SokobanPuzzle] -> IO ()
printSolution solutionStack = do
    putStrLn "Solution found:"
    printIntermediateStates solutionStack
    putStrLn "Puzzle solved."

-- Prints the direction the player has gone between two puzzle states
printIntermediateStates :: [SokobanPuzzle] -> IO ()
printIntermediateStates [] = return ()
printIntermediateStates [finalState] = do
    putStrLn "Final State:"
    printSokobanPuzzle finalState
printIntermediateStates (current:next:rest) = do
    let direction = showDirection current next
    putStrLn $ "Intermediate State: " ++ direction
    printSokobanPuzzle next
    printIntermediateStates (next:rest)

-- Determines the direction of movement between two puzzle states
showDirection :: SokobanPuzzle -> SokobanPuzzle -> String
showDirection current next = 
    case (playerRow current, playerCol current, playerRow next, playerCol next) of
        (rowCurr, colCurr, rowNext, colNext)
            | rowCurr == rowNext && colCurr < colNext -> "right"
            | rowCurr == rowNext && colCurr > colNext -> "Left"
            | rowCurr < rowNext && colCurr == colNext -> "Down"
            | rowCurr > rowNext && colCurr == colNext -> "Up"
            | otherwise -> "Unknown"
    where 
        playerRow (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
                                Just index -> index `div` length (head gameState)
                                Nothing -> -1
        playerCol (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
                                Just index -> index `mod` length (head gameState)
                                Nothing -> -1
--Main function
main :: IO ()
main = do
    args <- getArgs
    case args of
        ["test"] -> runTests
        [filePath] -> do
            puzzle <- readSokobanFromFile filePath
            case puzzle of
                Just sokoban -> do
                    putStrLn "Puzzle successfully read:"
                    putStrLn "Initial Puzzle State:"
                    printSokobanPuzzle sokoban
                    case solvePuzzle sokoban of
                        Just steps -> do
                            putStrLn "Moves taken to solve the puzzle:"
                            printSolution steps
                        Nothing ->
                            putStrLn "No solution found for this puzzle."
                Nothing -> putStrLn "Failed to read puzzle from file."
        _ -> putStrLn "Usage: ./Sokoban <input-file>"

-- Prints the state of a sokoban puzzle
printSokobanPuzzle :: SokobanPuzzle -> IO ()
printSokobanPuzzle (SokobanPuzzle gameState) =
    mapM_ putStrLn (map (concatMap showTile) gameState)
    where
        showTile :: TileType -> String
        showTile Wall = "X"
        showTile Empty = " "
        showTile Player = "P"
        showTile PlayerGoal = "O"
        showTile Box = "B"
        showTile BoxGoal = "H"
        showTile Goal = "G"