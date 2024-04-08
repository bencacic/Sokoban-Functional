module SokobanSolver where

import SokobanDataTypes
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

-- Function to solve the Sokoban puzzle
solvePuzzle :: SokobanPuzzle -> Maybe [SokobanPuzzle]
solvePuzzle startState = solve [(startState, [])] Set.empty

-- Function to solve the puzzle using iteration
solve :: [(SokobanPuzzle, [SokobanPuzzle])] -> Set SokobanPuzzle -> Maybe [SokobanPuzzle]
solve [] _ = Nothing
solve ((current, path):stack) visited
    | isSolved current = Just (reverse (current:path))
    | Set.member current visited = solve stack visited
    | otherwise =
        let nextStates = getPossibleMoves current visited
            visited' = Set.insert current visited
            newStack = [(nextState, current:path) | nextState <- nextStates] ++ stack
        in solve newStack visited'

-- Function to check if the puzzle is solved
isSolved :: SokobanPuzzle -> Bool
isSolved (SokobanPuzzle gameState) = notElem Goal (concat gameState) && notElem PlayerGoal (concat gameState)

-- Function to get possible moves from the current state
getPossibleMoves :: SokobanPuzzle -> Set SokobanPuzzle -> [SokobanPuzzle]
getPossibleMoves puzzle visited =
    [newPuzzle | dir <- [Up, Down, SLeft, SRight]
               , let newPuzzle = movePlayer puzzle dir visited
               , isValidMove newPuzzle visited]

-- Function to move the player in a specified direction
movePlayer :: SokobanPuzzle -> Direction -> Set SokobanPuzzle -> SokobanPuzzle
movePlayer (SokobanPuzzle gameState) direction visited =
    let playerPos = getPlayerPosition gameState
        newPos = moveDirection playerPos direction
        currentTile = getTileAt playerPos gameState
        newTile = getTileAt newPos gameState
        newGameState =
            case (currentTile, newTile) of
                -- Player moves to an empty tile or goal
                (Player, Empty) -> updateGameState playerPos Empty (updateGameState newPos Player gameState)
                (Player, Goal) -> updateGameState playerPos Empty (updateGameState newPos PlayerGoal gameState)
                -- Player tries to move a box
                (Player, Box) ->
                    let boxNewPos = moveDirection newPos direction
                        boxNewTile = getTileAt boxNewPos gameState
                    in if boxNewTile == Empty || boxNewTile == Goal
                        then updateGameState playerPos Empty (updateGameState newPos Player (updateGameState boxNewPos (if boxNewTile == Goal then BoxGoal else Box) gameState))
                        else gameState
                (Player, BoxGoal) ->
                    let boxNewPos = moveDirection newPos direction
                        boxNewTile = getTileAt boxNewPos gameState
                    in if boxNewTile == Empty
                        then updateGameState playerPos Empty (updateGameState newPos PlayerGoal (updateGameState boxNewPos Box gameState))
                        else gameState
                -- Player cannot move to the new tile
                _ -> gameState
    in SokobanPuzzle newGameState



-- Function to check if a move is valid
isValidMove :: SokobanPuzzle -> Set SokobanPuzzle -> Bool
isValidMove puzzle visited =
    notElem puzzle visited

-- Function to get the position of the player in the game state
getPlayerPosition :: [[TileType]] -> (Int, Int)
getPlayerPosition gameState = 
    let rows = length gameState
        cols = length (head gameState)
        playerPositions = [(i, j) | i <- [0..rows-1], j <- [0..cols-1], getTileAt (i, j) gameState == Player || getTileAt (i, j) gameState == PlayerGoal]
    in if null playerPositions
        then (-1, -1) -- Player not found
        else head playerPositions

-- Function to get the tile type at a given position in the game state
getTileAt :: (Int, Int) -> [[TileType]] -> TileType
getTileAt (x, y) gameState
    | x < 0 || y < 0 || y >= length gameState || x >= length (head gameState) = Wall
    | otherwise = (gameState !! y) !! x

-- Function to update the game state with a new tile at a given position
updateGameState :: (Int, Int) -> TileType -> [[TileType]] -> [[TileType]]
updateGameState (x, y) tileType gameState =
    let (before, row:after) = splitAt y gameState
        newRow = replaceAtIndex x tileType row
    in before ++ newRow : after

-- Function to replace an element at a specific index in a list
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex i x (y:ys)
    | i == 0 = x : ys
    | otherwise = y : replaceAtIndex (i - 1) x ys

-- Function to move in a specified direction
moveDirection :: (Int, Int) -> Direction -> (Int, Int)
moveDirection (x, y) Up = (x, y - 1)
moveDirection (x, y) Down = (x, y + 1)
moveDirection (x, y) SLeft = (x - 1, y)
moveDirection (x, y) SRight = (x + 1, y)

-- Define the findIndex function
findIndex :: Eq a => a -> [a] -> Maybe Int
findIndex _ [] = Nothing
findIndex x (y:ys)
    | x == y = Just 0
    | otherwise = fmap (+1) (findIndex x ys)