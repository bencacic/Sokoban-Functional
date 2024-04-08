import SokobanDataTypes
import SokobanSolver
import SokobanInput
import SokobanTest(runTests)
import System.Environment (getArgs)

-- Function to print the puzzle solution
printSolution :: [SokobanPuzzle] -> IO ()
printSolution solutionStack = do
    putStrLn "Solution found:"
    printIntermediateStates solutionStack
    putStrLn "Puzzle solved."

-- Function to print the intermediate states of the puzzle with directions
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

-- Function to print the directions between puzzle states
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