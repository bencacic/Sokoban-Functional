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
  isSolved (SokobanPuzzle gameState) = notElem Goal (concat gameState) && notElem PlayerGoal (concat gameState)

  -- filter out visited states from set produced from possible moves
  getPossibleMoves :: SokobanPuzzle -> Set SokobanPuzzle -> [SokobanPuzzle]
  getPossibleMoves puzzle visited = filter (`Set.notMember` visited)
                   $ mapMaybe (\dir -> movePlayer puzzle dir) [Up, Down, SLeft, SRight]

  movePlayer :: SokobanPuzzle -> Direction -> Maybe SokobanPuzzle
  movePlayer (SokobanPuzzle gameState) direction =
      case findPlayer gameState of
          Just oldPlayerPos ->
              let newPlayerPos = moveDirection oldPlayerPos direction
                  oldTileType = getTileAt oldPlayerPos gameState
                  newTileType = getTileAt newPlayerPos gameState
                  isValid = isValidMove newPlayerPos direction gameState
              in if isValid
                  then
                      let (oldPlayerTile, newPlayerTile, newGameState) = case oldTileType of
                              PlayerGoal -> case newTileType of
                                  Empty -> (Goal, Player, gameState)
                                  Goal -> (Goal, PlayerGoal, gameState)
                                  Box -> let newState = moveBox newPlayerPos direction gameState
                                         in (Goal, Player, newState)
                                  BoxGoal -> let newState = moveBox newPlayerPos direction gameState
                                             in (Goal, PlayerGoal, newState)
                                  _ -> (PlayerGoal, newTileType, gameState) -- invalid move
                              _ -> case newTileType of
                                  Empty -> (Empty, Player, gameState)
                                  Goal -> (Empty, PlayerGoal, gameState)
                                  Box -> let newState = moveBox newPlayerPos direction gameState
                                         in (Empty, Player, newState)
                                  BoxGoal -> let newState = moveBox newPlayerPos direction gameState
                                             in (Empty, PlayerGoal, newState)
                                  _ -> (Player, newTileType, gameState) --invalid move
                          updatedGameState = updateGameState newPlayerPos newPlayerTile (updateGameState oldPlayerPos oldPlayerTile newGameState)
                      in Just (SokobanPuzzle updatedGameState)
                  else Just (SokobanPuzzle gameState)
          Nothing -> Nothing

  findPlayer :: [[TileType]] -> Maybe (Int, Int)
  findPlayer [] = Nothing
  findPlayer rows =
      case findPlayer' rows 0 of
          Just (colIndex, rowIndex) -> Just (colIndex, rowIndex)
          Nothing -> Nothing
      where
          findPlayer' :: [[TileType]] -> Int -> Maybe (Int, Int)
          findPlayer' [] _ = Nothing
          findPlayer' (row:restRows) rowIndex =
              case findIndex Player row of
                  Just colIndex -> Just (colIndex, rowIndex)
                  Nothing ->
                      case findIndex PlayerGoal row of
                          Just colIndex -> Just (rowIndex, colIndex)
                          Nothing -> findPlayer' restRows (rowIndex + 1)

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
  moveDirection (x, y) SLeft = (x - 1, y)
  moveDirection (x, y) SRight = (x + 1, y)

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
          Empty -> updateGameState newPos Box (updateGameState boxPos Empty gameState)
          Goal -> updateGameState newPos BoxGoal (updateGameState boxPos Empty gameState)
          _ -> gameState -- invalid move

  updateGameState :: (Int, Int) -> TileType -> [[TileType]] -> [[TileType]]
  updateGameState (col, row) tileType gameState =
      let updatedRow = replaceAtIndex col tileType (gameState !! row)
      in replaceAtIndex row updatedRow gameState

  isValidMove :: (Int, Int) -> Direction -> [[TileType]] -> Bool
  isValidMove newPos direction gameState =
          getTileAt newPos gameState /= Wall && not (isBoxCollision newPos direction gameState)

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
          (Box, Wall) -> True
          (BoxGoal, Wall) -> True
          _ -> False