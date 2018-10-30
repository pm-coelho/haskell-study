{-# Language OverloadedStrings #-}
import CodeWorld

Botcircle :: Color -> Picture
botCircle c = colored c (translated 0 (-1.5) (solidCircle 1))

topCircle :: Color -> Picture
topCircle c = colored c (translated 0 1.5 (solidCircle 1))

frame :: Picture
frame = rectangle 2.5 5.5

trafficLight :: Bool -> Picture
trafficLight True = botCircle green & topCircle black & frame
trafficLight False = botCircle black & topCircle red & frame

trafficController :: Double -> Picture
trafficController t
  | round (t/3) `mod`2 == 0 = trafficLight True
  | otherwise = trafficLight False

spread :: Picture -> Double -> Integer -> Picture
spread pic dx 0 = blank
spread pic dx n = pic & translated dx 0 (spread pic dx (n-1))

tree :: Integer -> Picture
tree 0 = blank
tree n = path [(0,0),(0,1)] & translated 0 1 (rotated (pi/10) (tree (n-1)) & rotated (- pi/10) (tree (n-1)))

ourPicture :: Picture
ourPicture = spread (trafficLight True) 3 4

main :: IO ()
main = drawingOf (tree 8)
