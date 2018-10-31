{-# Language OverloadedStrings #-}
import CodeWorld

wall :: Picture
wall = colored purple (solidRectangle 1 1)

ground :: Picture
ground = colored pink (solidRectangle 1 1)

storage :: Picture
storage = colored aquamarine (solidRectangle 1 1)

box :: Picture
box = colored green (solidRectangle 1 1)

drawTile :: Integer -> Picture
drawTile 0 = wall
drawTile 1 = ground
drawTile 2 = storage
drawTile 3 = box
drawTile _ = blank

maze :: Integer -> Integer -> Integer
maze x y
  | abs x > 4 || abs y > 4 = 0
  | abs x == 4 || abs y == 4 = 1
  | x == 2 && y <= 0 = 1
  | x == 3 && y <= 0 = 3
  | x >= -2 && y == 0 = 4
  | otherwise = 2


pictureOfMaze :: Picture
pictureOfMaze =


exercise3 :: IO()
exercise3 = drawingOf pictureOfMaze


main :: IO ()
main = exercise3
