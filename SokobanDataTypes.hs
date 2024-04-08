 
module SokobanDataTypes (
  TileType(..),
  Direction(..),
  SokobanPuzzle(..)
) where

data TileType = Wall | Empty | Player | PlayerGoal | Box | BoxGoal | Goal deriving (Eq, Ord, Show)
data Direction = Up | Down | SLeft | SRight deriving (Eq, Show)
data SokobanPuzzle = SokobanPuzzle { gameState :: [[TileType]]} deriving (Eq, Ord, Show)