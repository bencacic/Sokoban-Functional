module SokobanMain where

  import SokobanDataTypes
  import SokobanInput (readSokobanFromFile)
  import SokobanSolver (solvePuzzle)
  import qualified Data.Set as Set

  main :: IO ()
  main = do
    maybePuzzle <- readSokobanFromFile
    case maybePuzzle of
        Just puzzle -> do
            let solution = solvePuzzle puzzle
            case solution of
                Just steps -> do
                    putStrLn "Solution found:"
                    mapM_ print steps
                Nothing ->
                    putStrLn "No solution found for this puzzle."
        Nothing ->
            putStrLn "Failed to read the puzzle from file."