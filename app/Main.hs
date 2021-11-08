module Main where

main :: IO ()
main = do putStrLn "What is 2 + 2?"
          x <- readLn
          if x == 4
              then putStrLn "You're right!"
              else putStrLn "You're wrong!"

-- basic
test :: (Integral x) => x -> String
test 7 = "Number"
test x = "Sorry"

factorial :: (Integral y) => y -> y
factorial 0 = 1
factorial n = n * factorial (n - 1)

charName :: Char -> String
charName 'a' = "Albert"
charName 'b' = "Broseph"
charName 'c' = "Cecil"

equals :: Eq a => a -> a -> Bool
equals x y = x == y

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 +y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' [] = error "Can't call head on an empty list, dummy!"
head' (x:_) = x





-- reduce O(n) to O(log n)
-- that type of tree:
--
--      v
--    /   \
--   v     v
--  / \   / \
-- v  v  v   v
-- a  a  a  / \
--         v   v
--         a   a

-- binary tree
data Tree v a = Leaf v a
            | Branch v (Tree v a) (Tree v a)

-- the leaves store the elements from left to right
toList :: Tree v a -> [a]
toList (Leaf _ a)     = [a]
toList (Branch _ x y) = toList x ++ toList y

-- annotations
tag :: Tree v a -> v
tag (Leaf v _)  = v
tag (Branch v _ _) = v

-- retrieves the leftmost element
--head :: Tree v a -> a
--head (Leaf _ a)     = a
--head (Branch _ x _) = head x


type Size = Int

--tag (Leaf  ..)       = 1
--tag (Branch .. x y)  = tag x + tag y

--leaf :: a => Tree Size a
--leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch(tag x + tag y) x y

--(!!) :: Tree Size a -> Int -> a
--(Leaf _ a)      !! 0 = a
--(Branch _ x y)  !! n
--    | n < tag x      = x !! n
--    | otherwise      = y !! (n - tag x)

