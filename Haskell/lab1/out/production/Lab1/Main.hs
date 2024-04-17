module Main (main) where

-- Oscar Dahlberg - 9703146994

lab1 :: [Int] -> Int -> IO ()
lab1 xs k = putStrLn (show xs) >> putStrLn (show k) >> putStrLn "Size  Firt index Last index  Sublist" >>
  printKSmallestSubArrays (kSmallestSubArrays xs k)

-- Prints all elements by recursively printing each element with the next function
printKSmallestSubArrays :: [(Int, [Int], (Int, Int))] -> IO ()
printKSmallestSubArrays [] = putStrLn ""
printKSmallestSubArrays (x:xs) = printKSmallestSubArray x >> printKSmallestSubArrays xs

-- Prints one element
printKSmallestSubArray :: (Int, [Int], (Int, Int)) -> IO ()
printKSmallestSubArray (s, xs, (i, j)) =
  putStr (show s ) >> printSpaces 5 >> putStr (show i ) >> printSpaces 10 >> putStr (show j ) >>
  printSpaces 11 >> putStr (show xs) >> putStrLn ""

-- Cant use replicate from prelude package :(
printSpaces :: Int -> IO ()
printSpaces 1 = putStr " "
printSpaces n = putStr " " >> printSpaces (n-1)

-- Gets the k smallest sub arrays format: (size, [sub array], (index of first element, index of last element))
kSmallestSubArrays :: [Int] -> Int -> [(Int, [Int], (Int, Int))]
kSmallestSubArrays xs k = take k (sortAndFixSubListWithIndexes xs)

-- Creates and then sorts the sub arrays
sortAndFixSubListWithIndexes :: [Int] -> [(Int, [Int], (Int, Int))]
sortAndFixSubListWithIndexes xs = insertionSort(createAllSubListsAndIndex xs)

-- Just insertion sort, begins with one element and then checks
-- the next element and either inserts it in front or after depending on if the
-- element is smaller or larger than the item
insertionSort :: [(Int, [Int], (Int, Int))] -> [(Int, [Int], (Int, Int))]
insertionSort [] = []
insertionSort (x:xs) = insertIntoSorted (insertionSort xs) x

-- Recursively goes through the sorted list and inputs the sub list when
-- the element after it is larger than the element
insertIntoSorted :: [(Int, [Int], (Int, Int))] -> (Int, [Int], (Int, Int)) -> [(Int, [Int], (Int, Int))]
insertIntoSorted [] n = [n]
insertIntoSorted (x:xs) n
      | first3Tuple n < first3Tuple x         = n : x : xs
      | otherwise     = x : insertIntoSorted xs n

-- Helper function to get first element from tuple
first3Tuple :: (s, xs, ij) -> s
first3Tuple (s, _, _) = s

-- Creates all sub lists and the start and last index of the list
createAllSubListsAndIndex :: [Int] -> [(Int, [Int], (Int, Int))]
createAllSubListsAndIndex xs = fullSubLists xs 1 (length xs)

-- Recursively computes all of the sub lists by calling 'prefixIndexSubLists'
-- and then removing the first element from the list
fullSubLists :: [Int] -> Int -> Int -> [(Int, [Int], (Int, Int))]
fullSubLists [xs] i j = [(xs, [xs], (i, j))]
fullSubLists xs i j = [(sum xs, xs, (i, j))] ++ prefixIndexSubLists (init xs) i (j-1) ++ fullSubLists (tail xs) (i+1) j

-- Recursively computes all of the sub lists containing the first element
-- And formats the output so we get everything we need for printout
prefixIndexSubLists :: [Int] -> Int -> Int -> [(Int, [Int], (Int, Int))]
prefixIndexSubLists [xs] i j = [(xs, [xs], (i, j))]
prefixIndexSubLists xs i j = [(sum xs, xs, (i, j))] ++ prefixIndexSubLists (init xs) i (j-1)

main :: IO ()
main = do
  putStrLn "---------------------------------------"
  lab1 [x*(-1)^x | x <- [1..100]] 15
  putStrLn "---------------------------------------"
  lab1 [24, -11, -34, 42, -24, 7, -19, 21] 6
  putStrLn "---------------------------------------"
  lab1 [3, 2, -4, 3, 2, -5, -2, 2, 3, -3, 2, -5, 6, -2, 2, 3] 8
  putStrLn "---------------------------------------"
  lab1 [-1,2,-3,4,-5] 3
  putStrLn "---------------------------------------"
  lab1 [] 10
