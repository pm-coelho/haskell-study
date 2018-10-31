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


trafficSign :: Bool -> Bool -> Bool -> Picture
trafficSign r a g = redCircle r & amberCircle a & greenCircle g & frame

trafficLight :: [Color] -> Picture
trafficLight c
  | c == [red] = trafficSign True False False
  | c == [yellow] = trafficSign False True False
  | c == [green] = trafficSign False False True
  | c == [red, yellow] = trafficSign True True False
  | otherwise = trafficSign False False False


trafficController :: Double -> Picture
trafficController t
  | time `elem` [0,1] = trafficLight [green]
  | time == 2 = trafficLight [yellow]
  | time `elem` [3,4] = trafficLight [red]
  | otherwise = trafficLight [red, yellow]
  where time = round (t/2) `mod` 6


exercise1 :: IO ()
exercise1 = animationOf trafficController

main :: IO ()
main = exercise1
