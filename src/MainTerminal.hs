-- A terminal-based driver and display for running Conway's Game of Life.

import GameOfLife
import Control.Concurrent            ( threadDelay )
import System.Console.ANSI           ( clearScreen )
import System.Console.Terminal.Size  ( Window(Window)
                                     , size )
import System.Random                 ( newStdGen )

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
  gen <- newStdGen
  (rows, cols) <- getTerminalSizeWithDefault 50
  let board = randomBoard rows cols gen
  mapM_ printAndWait $ iterate nextBoard board

