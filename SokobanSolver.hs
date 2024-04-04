module SokobanSolver where

  import SokobanDataTypes
  import Data.Set (Set)
  import qualified Data.Set as Set

  solvePuzzle :: SokobanPuzzle -> Maybe [SokobanPuzzle]
  solvePuzzle startState =
                let initialStateSet = Set.singleton startState
                in fst(solve [startState] initialStateSet)

  solve :: [SokobanPuzzle] -> Set SokobanPuzzle -> (Maybe [SokobanPuzzle], Set SokobanPuzzle)
  solve [] _ = (Nothing, _)
  solve (current:stack) visited --current is puzzle I'm looking at, stack is steps that brought me here, visited is puzzle states I've visited
        | isSolved current = (Just [current], visited) --current is the solution, so we return the stack we have
        | otherwise = exploreMoves (getPossibleMoves current visited) (current:stack) visited

  exploreMoves :: [SokobanPuzzle] -> [SokobanPuzzle] -> Set SokobanPuzzle -> (Maybe [SokobanPuzzle], Set SokobanPuzzle)
  exploreMoves [] _ visited = (Nothing, visited)
  exploreMoves (nextMove:possibleMoves) stack visited =
                    let setVisited = Set.insert nextMove visited
                        (solution, finalVisited) = solve (nextMove:stack) setVisited
                    in case solution of
                        Just moves -> (Just moves, finalVisited)
                        Nothing -> exploreMoves possibleMoves stack finalVisited

  isSolved :: SokobanPuzzle -> Bool
  isSolved puzzle = {-add logic for solution checking-}

  getPossibleMoves :: SokobanPuzzle -> Set SokobanPuzzle -> [SokobanPuzzle]
  getPossibleMoves puzzle = {-add logic for move generation, make sure to check that the move hasn't been visited-}