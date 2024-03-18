 
module SokobanDataTypes (
  TileType(..),
  Direction(..),
  SokobanPuzzle(..)
) where

data TileType = Wall | Empty | Player | PlayerGoal | Box | BoxGoal | Goal deriving (Eq, Show)
data Direction = Up | Down | Left | Right deriving (Eq, Show)
data SokobanPuzzle = SokobanPuzzle { gameState :: [[TileType]]} deriving (Eq, Show)