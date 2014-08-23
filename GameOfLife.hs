-- Conway's Game of Life in the terminal
-- Dependencies: ansi-terminal, terminal-size

module GameOfLife 
    ( State(Alive, Dead), Board(Board)
    , nextBoard) where

import System.Random

testAll :: Bool
testAll = all id [ testGetCell
                 , testGetNeighbors
                 , testGetNeighborsWrap
                 , testCountLiving
                 , testNextState ]

testBoard :: Board
testBoard = Board 3 3 [[Alive, Dead, Dead],
                       [Dead,  Dead, Alive],
                       [Alive, Dead, Dead]]

testGetCell :: Bool
testGetCell = 
  let p = (Point 1 1)
  in getCell testBoard p == Dead

testGetNeighbors :: Bool
testGetNeighbors = 
  let neighbors = [Alive, Dead, Dead, Dead, Alive, Alive, Dead, Dead]
      point = Point 1 1 
   in neighbors == (getNeighbors testBoard point)

testGetNeighborsWrap :: Bool
testGetNeighborsWrap = 
  let neighbors = [Dead, Alive, Dead, Dead, Dead, Alive, Dead, Dead]
      point = Point 0 0 
   in neighbors == (getNeighbors testBoard point)


testCountLiving :: Bool
testCountLiving =
  let cells = [Alive, Dead, Dead, Dead, Alive, Alive, Dead, Alive]
  in countLiving cells == 4

testNextState :: Bool
testNextState = nextState testBoard (Point 1 1) == Alive

testNextBoard :: Bool
testNextBoard = all (all (== Alive)) $ cells $ nextBoard testBoard

data State = Alive | Dead
	 deriving (Eq, Enum, Bounded)

instance Random State where
    randomR (a, b) g =
        case randomR (fromEnum a, fromEnum b) g of
          (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

instance Show State where
    show Alive = "O"
    show Dead  = " "

-- (row, col)
data Point = Point Int Int
             deriving (Eq, Show)

data Board = Board {
      numRows :: Int,
      numCols :: Int,
      cells :: [[State]]
}

instance Show Board where
    show = unlines . map unwords . (map $ map show) . cells

getCell :: Board -> Point -> State
getCell b (Point r c) = cells b !! r !! c

-- Ordered top left to bottom right.
getNeighbors :: Board -> Point -> [State]
getNeighbors b (Point row col) =
  let offsets = [(-1), 0, 1]
      rows = [r | r <- map (row +) offsets]
      neighborPoints = [Point (r `mod` numRows b) (c `mod` numCols b)
                        | c <- map (col +) offsets, r <- rows, r /= row || c /= col]
  in map (getCell b) neighborPoints

countLiving :: [State] -> Int
countLiving = length . filter (== Alive) 

nextState :: Board -> Point -> State
nextState b p = case (state, num_living_neighbors) of
                  (Alive, 2) -> Alive
                  (_, 3)     -> Alive
                  (_, _)     -> Dead
  where state                = getCell b p
        num_living_neighbors = countLiving $ getNeighbors b p

nextBoard :: Board -> Board
nextBoard b = Board (numRows b) (numCols b) [
               [nextState b (Point r c) | c <- [0..(pred $ numCols b)]]
                   | r <- [0..(pred $ numRows b)]]

