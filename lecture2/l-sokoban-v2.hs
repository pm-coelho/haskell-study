{-# Language OverloadedStrings #-}
import CodeWorld


-- DRAWING
data Tile = Wall | Ground | Storage | Box | Blank

wall, ground, storage, box :: Picture
wall = colored purple (solidRectangle 1 1)
ground = colored pink (solidRectangle 1 1)
storage = colored green (solidRectangle 1 1)
box = colored blue (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go 11 = blank
    go n = something n & go (n+1)

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (drawTileAt r))

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze r c))

maze :: Integer -> Integer -> Tile
maze x y
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

drawState :: Coord -> Picture
drawState c = atCoord c pictureOfMaze
-- END DRAWING


-- MOVEMENT
data Direction = R | U | L | D
data Coord = C Integer Integer

initialCoord :: Coord
initialCoord = C 0 0

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)
-- END MOVEMENT


-- TIME
handleTime :: Double -> Coord -> Coord
handleTime _ c = c
-- END TIME


-- EVENTS
handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
  | key == "Right" = adjacentCoord R c
  | key == "Up" = adjacentCoord U c
  | key == "Left" = adjacentCoord L c
  | key == "Down" = adjacentCoord D c
handleEvent _ c = c
-- END EVENTS


tilesGame :: IO()
tilesGame = interactionOf initialCoord handleTime handleEvent drawState

main :: IO ()
main = tilesGame
