{- |
Module      :  SokobanSolver
Description :  The solver for a Sokoban puzzle.

Creators  :  bcaci729@mtroyal.ca, kcaro419@mtroyal.ca, marab065@mtroyal.ca

-}
module SokobanSolver where

import SokobanDataTypes
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (mapMaybe)

-- Creates a set containing only the initial state of the puzzle
solvePuzzle :: SokobanPuzzle -> Maybe [SokobanPuzzle]
solvePuzzle startState = solve [(startState, [])] Set.empty

-- Logic for the search algorithm.
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

-- Determines if the puzzle is successfully solved
isSolved :: SokobanPuzzle -> Bool
isSolved (SokobanPuzzle gameState) = notElem Goal (concat gameState) && notElem PlayerGoal (concat gameState)

-- Gets possible moves from the current state, and checks them for validity
getPossibleMoves :: SokobanPuzzle -> Set SokobanPuzzle -> [SokobanPuzzle]
getPossibleMoves puzzle visited =
    [newPuzzle | dir <- [Up, Down, SLeft, SRight]
               , let newPuzzle = movePlayer puzzle dir visited
               , isValidMove newPuzzle visited]

-- Updates the players position.
movePlayer :: SokobanPuzzle -> Direction -> Set SokobanPuzzle -> SokobanPuzzle
movePlayer (SokobanPuzzle gameState) direction visited =
    let playerPos = getPlayerPosition gameState
        newPos = moveDirection playerPos direction
        currentTile = getTileAt playerPos gameState
        newTile = getTileAt newPos gameState
        (playerType, playerBelow) = case currentTile of
            Player -> (Player, Empty)
            PlayerGoal -> (PlayerGoal, Goal)
            _ -> (Empty, Empty)
        newGameState =
            if playerType == Player || playerType == PlayerGoal
            then case newTile of
                -- Player moves to an empty tile or goal
                Empty -> updateGameState playerPos playerBelow (updateGameState newPos Player gameState)
                Goal -> updateGameState playerPos playerBelow (updateGameState newPos PlayerGoal gameState)
                -- Player tries to move a box
                Box ->
                    let boxNewPos = moveDirection newPos direction
                        boxNewTile = getTileAt boxNewPos gameState
                    in if boxNewTile == Empty || boxNewTile == Goal
                        then updateGameState playerPos playerBelow (updateGameState newPos Player (updateGameState
                             boxNewPos (if boxNewTile == Goal then BoxGoal else Box) gameState))
                        else gameState
                BoxGoal ->
                    let boxNewPos = moveDirection newPos direction
                        boxNewTile = getTileAt boxNewPos gameState
                    in if boxNewTile == Empty
                        then updateGameState playerPos playerBelow (updateGameState newPos PlayerGoal
                             (updateGameState boxNewPos Box gameState))
                        else gameState
                -- Player cannot move to the new tile
                _ -> gameState
            else gameState
    in SokobanPuzzle newGameState



-- Determines if a move is valid or not
isValidMove :: SokobanPuzzle -> Set SokobanPuzzle -> Bool
isValidMove puzzle visited =
    notElem puzzle visited

-- Gets the position of the player in the game state
getPlayerPosition :: [[TileType]] -> (Int, Int)
getPlayerPosition gameState = 
    let rows = length gameState
        cols = length (head gameState)
        playerPositions = [(i, j) | i <- [0..rows-1], j <- [0..cols-1], getTileAt (i, j) gameState == Player ||
                          getTileAt (i, j) gameState == PlayerGoal]
    in if null playerPositions
        then (-1, -1) -- Player not found
        else head playerPositions

-- Gets the tile type at a given position in the game state
getTileAt :: (Int, Int) -> [[TileType]] -> TileType
getTileAt (x,y) gameState
    | x < 0 || y < 0 || y >= length gameState || x >= rowWidth = Wall
    | otherwise = row !! x
    where
        rowWidth = case drop y gameState of
            [] -> 0
            (row:_) -> length row
        row = gameState !! y

-- Updates the game state with a new tile at a given position
updateGameState :: (Int, Int) -> TileType -> [[TileType]] -> [[TileType]]
updateGameState (x, y) tileType gameState =
    let (before, row:after) = splitAt y gameState
        newRow = replaceAtIndex x tileType row
    in before ++ newRow : after

-- Replaces an element at a specific index in a list
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex i x (y:ys)
    | i == 0 = x : ys
    | otherwise = y : replaceAtIndex (i - 1) x ys

-- Moves in a specified direction
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