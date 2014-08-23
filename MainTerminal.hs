import GameOfLife

import Control.Concurrent(threadDelay)
import Data.List.Split(chunksOf)
import System.Console.ANSI(clearScreen)
import System.Console.Terminal.Size
import System.Random(randoms, newStdGen)

printAndWait :: Board -> IO()
printAndWait b = do
  clearScreen
  print b
  threadDelay 250000 -- microseconds
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

