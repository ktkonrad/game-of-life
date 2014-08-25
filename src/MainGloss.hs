-- Graphics!

import GameOfLife
import Graphics.Gloss ( Color
                      , Display(InWindow)
                      , Picture(Color, Pictures, Polygon, Translate)
                      , simulate
                      , black
                      , white )
import System.Random  ( newStdGen )

main :: IO ()
main = do
  let size = windowSize `div` round cellSize
  gen <- newStdGen
  simulate
       (InWindow windowTitle (windowSize, windowSize) windowPosition)
       backgroundColor
       stepsPerSecond
       (randomBoard size size gen)
       renderBoard
       (\_ _ b -> nextBoard b)

stepsPerSecond = 10 :: Int

windowTitle = "Game of Life"

windowSize = 700 :: Int

cellSize = 10 :: Float

windowPosition = (10, 10) :: (Int, Int)

backgroundColor = white

liveCellColor = black

renderBoard :: Board -> Picture
renderBoard board = 
    let offset = - fromIntegral windowSize / 2
    in Translate offset offset
           $ Pictures
           $ map (renderPoint . fst)
           $ filter ((== Alive) . snd)
           $ cellsWithPositions board

renderPoint :: Point -> Picture
renderPoint (Point row col) =
    let [(x1, x2), (y1, y2)] = [((fromIntegral n) * cellSize, (fromIntegral $ succ n) * cellSize) | n <- [col, row]]
    in Color liveCellColor
           $ Polygon [(x1, y1), (x1, y2), (x2, y2), (x2, y1)]
