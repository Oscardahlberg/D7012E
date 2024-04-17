module Main (main) where

import Lib

-- MAIN THEORY FOR FIRST PART: ITERATE THROUGH THE ARRAY AND FOR EVERY NUMBER
-- 

main :: IO ()
main = do
  print "---------------------------------------"
  lab1 [x*(-1)^x | x <- [1..100]] 15
  print "---------------------------------------"
  lab1 [24, -11, -34, 42, -24, 7, -19, 21] 6
  print "---------------------------------------"
  lab1 [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3] 8
  print "---------------------------------------"
  lab1 [-1,2,-3,4,-5] 3
  print "---------------------------------------"
  lab1 [] 10
