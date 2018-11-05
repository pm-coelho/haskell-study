{-# Language OverloadedStrings #-}
import CodeWorld


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

data Direction = R | U | L | D
data Coord = C Integer Integer

atCoord :: Coord -> Picture -> Picture
atCoord (C x y) pic = translated (fromIntegral x) (fromIntegral y) pic

adjacentCoord :: Direction -> Coord -> Coord
adjacentCoord R (C x y) = C (x+1) y
adjacentCoord U (C x y) = C x (y+1)
adjacentCoord L (C x y) = C (x-1) y
adjacentCoord D (C x y) = C x (y-1)

data Interaction world = Interaction
  world
  (Double -> world -> world)
  (Event -> world -> world)
  (world -> Picture)

runInteraction :: Interaction s -> IO ()
runInteraction (Interaction state0 step handle draw)
  = interactionOf state0 step handle draw

data List a = Empty | Entry a (List a)

someBoxCoords :: List Coord
someBoxCoords = Entry (C 2 2) (Entry (C 3 3) (Entry (C (-1) 0) Empty))

combine :: List Picture -> Picture
combine Empty = blank
combine (Entry p ps) = p & combine ps

boxes :: List Coord -> Picture
boxes cs = combine (mapList (\c -> atCoord c (drawTile Box)) cs)

movingBoxes :: Interaction (List Coord)
movingBoxes = Interaction someBoxCoords (\_ s -> s) handle draw
  where
    draw = boxes
    handle (KeyPress key) s
        | key == "Right" = mapList (adjacentCoord R) s
        | key == "Up"    = mapList (adjacentCoord U) s
        | key == "Left"  = mapList (adjacentCoord L) s
        | key == "Down"  = mapList (adjacentCoord D) s
    handle _ s      = s

mapList :: (a -> b) -> List a -> List b
mapList _ Empty = Empty
mapList f (Entry c cs) = Entry (f c) (mapList f cs)

main :: IO ()
main = runInteraction movingBoxes
