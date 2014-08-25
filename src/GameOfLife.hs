-- Conway's Game of Life.

module GameOfLife 
    ( Board(..)
    , State(..)
    , Point(..)
    , getCell
    , getNeighbors
    , countLiving
    , nextState
    , nextBoard) where

data State = Alive | Dead
     deriving (Eq, Enum, Bounded)
         
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
      neighborPoints = [Point (r `mod` numRows b) (c `mod` numCols b)
                       | r <- map (row +) offsets, c <- map (col +) offsets
                       , r /= row || c /= col]
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
