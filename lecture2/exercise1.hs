{-# Language OverloadedStrings #-}
import CodeWorld


-- MAZE DRAWING
data Tile = Wall | Ground | Storage | Box | Blank

wall, ground, storage, box :: Picture
wall = colored purple (solidRectangle 1 1)
ground = colored pink (solidRectangle 1 1)
storage =
  colored green (solidCircle 0.2) &
  colored pink (solidRectangle 1 1)
box = colored blue (solidRectangle 1 1)

drawTile :: Tile -> Picture
drawTile Wall = wall
drawTile Ground = ground
drawTile Storage = storage
drawTile Box = box
drawTile Blank = blank

drawTileAt :: Integer -> Integer -> Picture
drawTileAt r c = translated (fromIntegral r) (fromIntegral c) (drawTile (maze (C r c)))

draw21times :: (Integer -> Picture) -> Picture
draw21times something = go (-10)
  where
    go 11 = blank
    go n = something n & go (n+1)

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (drawTileAt r))
-- END MAZE DRAWING


-- PLAYER DRAWING
player :: Picture
player = polyline [(0,0), (0.4,0)] & translated 0 0 (circle 0.4)
-- END PLAYER DRAWING


-- MOVEMENT
data Direction = R | U | L | D
data Coord = C Integer Integer

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

handleEvent :: Event -> Coord -> Coord
handleEvent (KeyPress key) c
  | key == "Right" = tryMove c R
  | key == "Up" = tryMove c U
  | key == "Left" = tryMove c L
  | key == "Down" = tryMove c D
handleEvent _ c = c

tryMove :: Coord -> Direction -> Coord
tryMove from d
  | canMove (maze to) = to
  | otherwise = from
  where to = adjacentCoord d from

canMove :: Tile -> Bool
canMove Ground = True
canMove Storage = True
canMove _ = False
-- END MOVEMENT


drawState :: Coord -> Picture
drawState c = atCoord c player & pictureOfMaze

exercise1 :: IO()
exercise1 = interactionOf (C 0 1) (\_ c -> c) handleEvent drawState

main :: IO ()
main = exercise1
