module SokobanSolver where

  import SokobanDataTypes
  import Data.Set (Set)
  import Data.Maybe (mapMaybe)
  import qualified Data.Set as Set

  solvePuzzle :: SokobanPuzzle -> Maybe [SokobanPuzzle]
  solvePuzzle startState =
                let initialStateSet = Set.singleton startState
                in fst(solve [startState] initialStateSet)

  solve :: [SokobanPuzzle] -> Set SokobanPuzzle -> (Maybe [SokobanPuzzle], Set SokobanPuzzle)
  solve [] _ = (Nothing, Set.empty)
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

  isSolved :: SokobanPuzzle -> Bool --concat the gameState into a single list, check it for goals
  isSolved (SokobanPuzzle gameState) = notElem Goal concatState && notElem PlayerGoal concatState
      where
          concatState = concat gameState

  -- filter out visited states from set produced from possible moves
  getPossibleMoves :: SokobanPuzzle -> Set SokobanPuzzle -> [SokobanPuzzle]
  getPossibleMoves puzzle visited = filter (`Set.notMember` visited)
                   $ mapMaybe (\dir -> movePlayer puzzle dir) [Up, Down, SokobanDataTypes.Left, SokobanDataTypes.Right]

  movePlayer :: SokobanPuzzle -> Direction -> Maybe SokobanPuzzle
  movePlayer (SokobanPuzzle gameState) direction =
      case findPlayer gameState of
          Just (oldPlayerPos, newGameState) ->
              let newPlayerPos = moveDirection oldPlayerPos direction
                  oldTileType = getTileAt oldPlayerPos newGameState
                  newTileType = getTileAt newPlayerPos newGameState
                  newPos = moveDirection newPlayerPos direction
                  isValid = isValidMove newPos direction gameState
              in if isValid
                  then
                      let (movedPlayer, movedTile, newGameState2) = case oldTileType of
                              PlayerGoal -> case newTileType of
                                  Empty -> (Goal, Player, newGameState)
                                  Goal -> (Goal, PlayerGoal, newGameState)
                                  Box -> let newState = moveBox newPlayerPos direction newGameState
                                         in (Goal, Player, newState)
                                  BoxGoal -> let newState = moveBox newPlayerPos direction newGameState
                                             in (Goal, PlayerGoal, newState)
                                  _ -> (PlayerGoal, newTileType, newGameState) -- invalid move
                              _ -> case newTileType of
                                  Empty -> (Empty, Player, newGameState)
                                  Goal -> (Empty, PlayerGoal, newGameState)
                                  Box -> let newState = moveBox newPlayerPos direction newGameState
                                         in (Empty, Player, newState)
                                  BoxGoal -> let newState = moveBox newPlayerPos direction newGameState
                                             in (Empty, PlayerGoal, newState)
                                  _ -> (Player, newTileType, newGameState) --invalid move
                          updatedGameState = updateGameState oldPlayerPos newPlayerPos (movedPlayer, movedTile) newGameState2
                      in Just (SokobanPuzzle updatedGameState)
                  else Nothing
          Nothing -> Nothing

  findPlayer :: [[TileType]] -> Maybe ((Int, Int), [[TileType]])
  findPlayer [] = Nothing
  findPlayer (row:rows) =
      case findIndex Player row of
          Just colIndex -> Just ((colIndex, length rows), row : rows)
          Nothing ->
              case findIndex PlayerGoal row of
                  Just colIndex -> Just ((colIndex, length rows), row : rows)
                  Nothing -> case findPlayer rows of
                      Just ((playerX, playerY), newRows) -> Just ((playerX, playerY - 1), row : newRows)
                      Nothing -> Nothing

  findIndex :: Eq a => a -> [a] -> Maybe Int
  findIndex _ [] = Nothing
  findIndex x (y:ys)
      | x == y = Just 0
      | otherwise = fmap (+1) (findIndex x ys)

  replaceAtIndex :: Int -> a -> [a] -> [a]
  replaceAtIndex _ _ [] = []
  replaceAtIndex 0 newValue (_:xs) = newValue : xs
  replaceAtIndex index newValue (x:xs) = x : replaceAtIndex (index - 1) newValue xs

  moveDirection :: (Int, Int) -> Direction -> (Int, Int)
  moveDirection (x, y) Up = (x, y - 1)
  moveDirection (x, y) Down = (x, y + 1)
  moveDirection (x, y) SokobanDataTypes.Left = (x - 1, y)
  moveDirection (x, y) SokobanDataTypes.Right = (x + 1, y)

  getTileAt :: (Int, Int) -> [[TileType]] -> TileType
  getTileAt (x,y) gameState
      | x < 0 || y < 0 || y >= length gameState || x >= rowWidth = Wall
      | otherwise = row !! x
      where
          rowWidth = case drop y gameState of
              [] -> 0
              (row:_) -> length row
          row = gameState !! y

  moveBox :: (Int, Int) -> Direction -> [[TileType]] -> [[TileType]]
  moveBox boxPos direction gameState =
      let newPos = moveDirection boxPos direction
          newTileType = getTileAt newPos gameState
      in case newTileType of
          Empty -> updateGameState boxPos newPos (Empty, Box) gameState
          Goal -> updateGameState boxPos newPos (Empty, BoxGoal) gameState
          _ -> gameState -- invalid move

  updateGameState :: (Int, Int) -> (Int, Int) -> (TileType, TileType) -> [[TileType]] -> [[TileType]]
  updateGameState (oldRow, oldCol) (newRow, newCol) (oldTileType, newTileType) gameState =
      let updatedRowOld = replaceAtIndex oldCol oldTileType (gameState !! oldRow)
          updatedRowNew = replaceAtIndex newCol newTileType (gameState !! newRow)
      in replaceAtIndex oldRow updatedRowOld (replaceAtIndex newRow updatedRowNew gameState)

  isValidMove :: (Int, Int) -> Direction -> [[TileType]] -> Bool
  isValidMove newPos direction gameState =
          newPos `notElem` wallPositions
              && not (isBoxCollision newPos direction gameState)
      where
          wallPositions = [(x, y) | (y, row) <- zip [0..] gameState, (x, tileType) <- zip [0..] row, tileType == Wall]

  isBoxCollision :: (Int, Int) -> Direction -> [[TileType]] -> Bool
  isBoxCollision newPos direction gameState =
      let newNewPos = moveDirection newPos direction
          currentTileType = getTileAt newPos gameState
          newTileType = getTileAt newNewPos gameState
      in case (currentTileType, newTileType) of
          (Box, Box) -> True
          (Box, BoxGoal) -> True
          (BoxGoal, Box) -> True
          (BoxGoal, BoxGoal) -> True
          _ -> False