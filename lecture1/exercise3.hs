{-# Language OverloadedStrings #-}
import CodeWorld

wall, ground, storage, box :: Picture
wall = colored purple (solidRectangle 1 1)
ground = colored pink (solidRectangle 1 1)
storage = colored green (solidRectangle 1 1)
box = colored blue (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 1 = wall
drawTile 2 = ground
drawTile 3 = storage
drawTile 4 = box
drawTile _ = blank

pictureOfMaze :: Picture
pictureOfMaze = drawRows (-10)

drawRows :: Integer -> Picture
drawRows 11 = blank
drawRows r = drawCols r (-10) & drawRows (r + 1)

drawCols :: Integer -> Integer -> Picture
drawCols _ 11 = blank
drawCols r c = drawTileAt r c & drawCols r (c + 1)

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))


maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4 || abs y > 4 = 0
  | abs x == 4 || abs y == 4 = 1
  | x == 2 && y <= 0 = 1
  | x == 3 && y <= 0 = 3
  | x >= -2 && y == 0 = 4
  | otherwise = 2

exercise3 :: IO()
exercise3 = drawingOf pictureOfMaze

main :: IO ()
main = exercise3
