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

maze0 :: Coord -> Tile
maze0 (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 2 && y <= 0 = Wall
  | x == 3 && y <= 0 = Storage
  | x >= -2 && y == 0 = Box
  | otherwise = Ground

maze :: Coord -> Tile
maze (C x y)
  | abs x > 4 || abs y > 4 = Blank
  | abs x == 4 || abs y == 4 = Wall
  | x == 0 && y < 0 = Wall
  | x > 0 && y == 2 = Wall
  | x == 1 && y == 1 = Wall
  | x == -3 && y == 3 = Storage
  | x == -3 && y == 2 = Storage
  | x == 3 && y == 3 = Storage
  | x == 3 && y == -3 = Storage
  | x < 0 && y == 0 = Box
  | x == -2 && y == 1 = Box
  | otherwise = Ground

pictureOfMaze :: Picture
pictureOfMaze = draw21times (\r -> draw21times (drawTileAt r))
-- END MAZE DRAWING

-- PLAYER DRAWING
player :: Picture
player = polyline [(0,0), (0.4,0)] & translated 0 0 (circle 0.4)

player2 :: Direction -> Picture
player2 R = player
player2 L = rotated (2 * 1.570796) player
player2 U = rotated (1 * 1.570796) player
player2 D = rotated (3 * 1.570796) player
-- END PLAYER DRAWING


-- MOVEMENT
data Direction = R | U | L | D
data Coord = C Integer Integer
data State = State Coord Direction


atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

handleEvent :: Event -> State -> State
handleEvent (KeyPress key) s
  | key == "Right" = tryMove s R
  | key == "Up" = tryMove s U
  | key == "Left" = tryMove s L
  | key == "Down" = tryMove s D
handleEvent _ s = s

tryMove :: State -> Direction -> State
tryMove (State from _) d
  | canMove (maze to) = State to d
  | otherwise = State from d
  where to = adjacentCoord d from

canMove :: Tile -> Bool
canMove Ground = True
canMove Storage = True
canMove _ = False
-- END MOVEMENT

-- GAME SETUP
initialState :: State
initialState = State (C 0 1) R

drawState :: State -> Picture
drawState (State c d) = atCoord c (player2 d) & pictureOfMaze

exercise3 :: IO()
exercise3 =
  resetableInteractionOf initialState (\_ c -> c) handleEvent drawState

resetableInteractionOf ::
  world ->
  (Double -> world -> world) ->
  (Event -> world -> world) ->
  (world -> Picture) ->
  IO ()
resetableInteractionOf state0 step handle draw
  = interactionOf state0 step handle' draw
  where handle' (KeyPress key) _ | key == "Esc" = state0
        handle' e s = handle e s


main :: IO ()
main = exercise3
-- END GAME SETUP
