# Sokoban-Functional
Functional implementation of a Sokoban solver written in Haskell. Sokoban is a puzzle game, where one is given a limited, maze-like space where the goal is to push a set of one or more boxes into specific positions within the maze. This implementation follows a Depth-First Search. 


The solver takes in the starting state of the puzzle as input, and produces either a set of steps (specifically directions the player takes) to solve the problem, or in the case where the puzzle is unsolvable, returns that fact.

The output is a list of directions the player has taken to successfully solve the puzzle, alongside the total number of steps this solution requires. This solution is **not** the optimal solution, but rather the first successful one. In the case where there is no valid solution, the program will output that.
