module Main (main) where

import TestExpr
import TestStatement
import TestParser
import TestProgram

main :: IO ()
main = do

    -- Test Program
    putStrLn $ "rp: " ++ show rp
    putStrLn $ "rp1: " ++ show rp1
    -- sp
    putStrLn $ "rp2: " ++ show rp2
    putStrLn $ "rp3: " ++ show rp3
    putStrLn $ "rp4: " ++ show rp4
    putStrLn $ "rp5: " ++ show rp5

    -- Test Statement

    --putStrLn $ "p1: " ++ show p1
    --putStrLn $ "p2: " ++ show p2
    --putStrLn $ "p3: " ++ show p3
    --putStrLn $ "p4: " ++ show p4
    --putStrLn $ "p5: " ++ show p5
    --putStrLn $ "p6: " ++ show p6
    --putStrLn $ "p7: " ++ show p7
    --putStrLn $ "p8: " ++ show p8
    --putStrLn $ "p9: " ++ show p9
    --putStrLn $ "p10: " ++ show p10
    putStrLn $ "p12: " ++ show p12
    --putStrLn $ "p13: " ++ show p13

  -- Test Parser
  
    putStrLn $ "Just('a',\"bc\"): " ++ show l1
    putStrLn $ "Nothing: " ++ show l2 {-
  putStrLn $ "Nothing: " ++ show l3

  putStrLn $ "Just(\"\",\"abc\"): " ++ show w1
  putStrLn $ "Just(\"  \\t \",\"abc\"): " ++ show w2

  putStrLn $ "Just (\"ab\",\"c\"): " ++ show c1
  putStrLn $ "Just (\"\",\"ab\"): " ++ show c2
  putStrLn $ "Nothing: " ++ show c3

  putStrLn $ "Just (\":=\",\"1\"): " ++ show r1
  putStrLn $ "error: " ++ show r2

  putStrLn $ "Just (\"count\",\"\"): " ++ show a4 -}

  -- Test Expr
  
  --putStrLn $ "n1: " ++ show n1
  --putStrLn $ "n2: " ++ show n2
  --putStrLn $ "n3: " ++ show n3
  --putStrLn $ "n4: " ++ show n4
  -- putStrLn $ "n21: " ++ show n21
  -- putStrLn $ "n31: " ++ show n31
