module Main where
import DataStructures.BinaryTree
import DataStructures.PriorityQueue
import Basic.Recursion
import Basic.HigherOrderFunctions

-- some compilation errors (not crucial at this time)
-- import DataStructures.Monoids
-- import DataStructures.Search

main :: IO ()
main = do putStrLn "What is 2 + 2?"
          x <- readLn
          if x == 4
              then putStrLn "You're right!"
              else putStrLn "You're wrong!"




