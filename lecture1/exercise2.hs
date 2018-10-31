{-# Language OverloadedStrings #-}
import CodeWorld

tree :: Picture -> Integer -> Picture
tree b 0 = b
tree b n =
  path [(0,0), (0,1)] &
  translated 0 1 (rotated (pi/10) (tree b (n-1)) &
  rotated (- pi/10) (tree b (n-1)))


blossom :: Double -> Picture
blossom t = colored yellow (solidCircle ((min t 10)/50))

treeAnimation :: Double -> Picture
treeAnimation t = tree (blossom t) 8

exercise2 :: IO ()
exercise2 = animationOf treeAnimation

main :: IO ()
main = exercise2
