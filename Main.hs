import SokobanDataTypes
import SokobanSolver
import SokobanInput
import SokobanTest(runTests)
import System.Environment (getArgs)

-- -- Function to print the puzzle solution
-- printSolution :: [SokobanPuzzle] -> IO ()
-- printSolution solutionStack = do
--     putStrLn "Solution found:"
--     printDirections solutionStack
--     putStrLn "Puzzle solved."

-- -- Function to print the directions between puzzle states
-- printDirections :: [SokobanPuzzle] -> IO ()
-- printDirections [] = return ()
-- printDirections [_] = return ()
-- printDirections (current:prev:rest) = do
--     putStrLn (showDirection current prev)
--     printDirections (prev:rest)

-- -- Function to determine the direction of movement between two puzzle states
-- showDirection :: SokobanPuzzle -> SokobanPuzzle -> String
-- showDirection current prev = 
--     case (playerRow current, playerCol current, playerRow prev, playerCol prev) of
--         (rowCurr, colCurr, rowPrev, colPrev)
--             | rowCurr == rowPrev && colCurr < colPrev -> "Left"
--             | rowCurr == rowPrev && colCurr > colPrev -> "Right"
--             | rowCurr < rowPrev && colCurr == colPrev -> "Up"
--             | rowCurr > rowPrev && colCurr == colPrev -> "Down"
--             | otherwise -> "Unknown"
--     where 
--         playerRow (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
--                                 Just index -> index `div` length (head gameState)
--                                 Nothing -> -1
--         playerCol (SokobanPuzzle gameState) = case findIndex Player (concat gameState) of
--                                 Just index -> index `mod` length (head gameState)
--                                 Nothing -> -1
                                
-- --Main function
-- main :: IO ()
-- main = do
--     args <- getArgs
--     case args of
--         ["test"] -> runTests
--         [filePath] -> do
--             puzzle <- readSokobanFromFile filePath
--             case puzzle of
--                 Just sokoban -> do
--                     putStrLn "Puzzle successfully read:"
--                     putStrLn "Initial Puzzle State:"
--                     printSokobanPuzzle sokoban
--                     case solvePuzzle sokoban of
--                         Just steps -> do
--                             putStrLn "Moves taken to solve the puzzle:"
--                             printSolution steps
--                         Nothing ->
--                             putStrLn "No solution found for this puzzle."
--                 Nothing -> putStrLn "Failed to read puzzle from file."
--         _ -> putStrLn "Usage: ./Sokoban <input-file>"

-- printSokobanPuzzle :: SokobanPuzzle -> IO ()
-- printSokobanPuzzle (SokobanPuzzle gameState) =
--     mapM_ putStrLn (map (concatMap showTile) gameState)
--     where
--         showTile :: TileType -> String
--         showTile Wall = "X"
--         showTile Empty = " "
--         showTile Player = "P"
--         showTile PlayerGoal = "O"
--         showTile Box = "B"
--         showTile BoxGoal = "H"
--         showTile Goal = "G"

-- Function to print the puzzle solution
printSolution :: [SokobanPuzzle] -> IO ()
printSolution solutionStack = do
    putStrLn "Solution found:"
    printStatesAndDirections solutionStack
    putStrLn "Puzzle solved."

-- Function to print the puzzle states along with the directions between them
printStatesAndDirections :: [SokobanPuzzle] -> IO ()
printStatesAndDirections [] = return ()
printStatesAndDirections [state] = putStrLn ("Final State:\n" ++ showSokobanPuzzle state)
printStatesAndDirections (current:prev:rest) = do
    putStrLn ("Direction: " ++ showDirection current prev)
    putStrLn ("State:\n" ++ showSokobanPuzzle prev)
    printStatesAndDirections (prev:rest)

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

-- Function to print a Sokoban puzzle
showSokobanPuzzle :: SokobanPuzzle -> String
showSokobanPuzzle (SokobanPuzzle gameState) =
    unlines $ map (concatMap showTile) gameState
    where
        showTile :: TileType -> String
        showTile Wall = "X"
        showTile Empty = " "
        showTile Player = "P"
        showTile PlayerGoal = "O"
        showTile Box = "B"
        showTile BoxGoal = "H"
        showTile Goal = "G"

-- Main function
main :: IO ()
main = do
    putStrLn "Testing readSokobanFromFile..."
    puzzle <- readSokobanFromFile filePath
    case puzzle of
        Just sokoban -> do
            putStrLn "Puzzle successfully read:"
            putStrLn "Initial Puzzle State:"
            putStrLn (showSokobanPuzzle sokoban)
            case solvePuzzle sokoban of
                Just steps -> do
                    putStrLn "Moves taken to solve the puzzle:"
                    printSolution steps
                Nothing ->
                    putStrLn "No solution found for this puzzle."
        Nothing -> putStrLn "Failed to read puzzle from file."