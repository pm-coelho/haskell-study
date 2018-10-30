{-# Language OverloadedStrings #-}
import CodeWorld

tLight :: Color -> Double -> Picture
tLight c x = colored c (translated 0 x (solidCircle 1))


redCircle :: Bool -> Picture
redCircle True = tLight red 2.5
redCircle False = tLight black 2.5

amberCircle :: Bool -> Picture
amberCircle True = tLight yellow 0
amberCircle False = tLight black 0

greenCircle :: Bool -> Picture
greenCircle True = tLight green (-2.5)
greenCircle False = tLight black (-2.5)

frame :: Picture
frame = rectangle 4.5 7.5


trafficLight :: Bool -> Bool -> Bool -> Picture
trafficLight r a g = redCircle r & amberCircle a & greenCircle g & frame

trafficController :: Double -> Picture
trafficController t
  | time `elem` [0,1] = trafficLight False False True
  | time == 2 = trafficLight False True False
  | time `elem` [3,4] = trafficLight True False False
  | otherwise = trafficLight True True False
  where time = round (t/2) `mod` 6


exercise1 :: IO ()
exercise1 = animationOf trafficController

main :: IO ()
main = exercise1
