{- |
Module      :  SokobanDataTypes
Description :  Represents a Sokoban puzzle, as well as the player movement options within that puzzle.

Creators  :  bcaci729@mtroyal.ca, kcaro419@mtroyal.ca, marab065@mtroyal.ca

-}
module SokobanDataTypes (
  TileType(..),
  Direction(..),
  SokobanPuzzle(..)
) where

-- All of the objects that can exist in a puzzle state
data TileType = Wall | Empty | Player | PlayerGoal | Box | BoxGoal | Goal deriving (Eq, Ord, Show)
-- The possible directions that the player can take
data Direction = Up | Down | SLeft | SRight deriving (Eq, Show)
-- A sokoban puzzle state
data SokobanPuzzle = SokobanPuzzle { gameState :: [[TileType]]} deriving (Eq, Ord, Show)