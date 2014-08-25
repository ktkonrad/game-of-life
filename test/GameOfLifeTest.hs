-- Tests for Game of Life.

module Main ( main ) where 

import GameOfLife

import Test.Framework (defaultMain)
import Test.Framework.Providers.HUnit (hUnitTestToTests)
import Test.HUnit( Counts
                 , Test(TestList)
                 , (~?)
                 , (~?=)
                 , runTestTT )

testBoard :: Board
testBoard = Board 3 3 [[Alive, Dead, Dead],
                       [Dead,  Dead, Alive],
                       [Alive, Dead, Dead]]

testGetCell = getCell testBoard (Point 1 1) ~?= Dead

testGetNeighbors =
    getNeighbors testBoard (Point 1 1) ~?= [Alive, Dead, Dead, Dead, Alive, Alive, Dead, Dead]

testGetNeighborsWrap = 
    getNeighbors testBoard (Point 0 0) ~?= [Dead, Alive, Dead, Dead, Dead, Alive, Dead, Dead]

testCountLiving = countLiving [Alive, Dead, Dead, Dead, Alive, Alive, Dead, Alive] ~?= 4
 
testNextState = nextState testBoard (Point 1 1) ~?= Alive

testNextBoard = (all (all (== Alive)) $ cells $ nextBoard testBoard) ~?
                "All cells are alive" 

testCellsWithPositions = (map fst $ cellsWithPositions testBoard) ~?=
                         [ Point 0 0, Point 0 1, Point 0 2
                         , Point 1 0, Point 1 1, Point 1 2
                         , Point 2 0, Point 2 1, Point 2 2 ]

testAll = TestList [ testGetCell
                   , testGetNeighbors
                   , testGetNeighborsWrap
                   , testCountLiving
                   , testNextState
                   , testNextBoard
                   , testCellsWithPositions ]

tests = hUnitTestToTests testAll

-- Used for running directly
main :: IO ()
main = defaultMain tests --runTestTT $ testAll

