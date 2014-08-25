-- A terminal-based driver and display for running Conway's Game of Life.

import GameOfLife

import Control.Concurrent(threadDelay)
import Data.List.Split(chunksOf)
import System.Console.ANSI(clearScreen)
import System.Console.Terminal.Size
import System.Random

instance Random State where
    randomR (a, b) g =
        case randomR (fromEnum a, fromEnum b) g of
          (x, g') -> (toEnum x, g')
    random g = randomR (minBound, maxBound) g

printAndWait :: Board -> IO()
printAndWait b = do
  clearScreen
  putStr $ show b    -- without newline
  threadDelay 100000 -- microseconds
  return ()

getTerminalSizeWithDefault :: Int -> IO (Int, Int)
getTerminalSizeWithDefault def = do
  windowSize <- size
  return (case windowSize of
           Just (Window rows cols) -> (pred rows, cols `div` 2)
           Nothing -> (def, def))

main :: IO()
main = do
  g <- newStdGen
  (rows, cols) <- getTerminalSizeWithDefault 50
  let bcells = take rows $ chunksOf cols $ (randoms g :: [State])
  let board = Board rows cols bcells
  mapM_ printAndWait $ iterate nextBoard board

