module Basic.Recursion where

-- http://learnyouahaskell.com/recursion


-- take ordered list - and return the biggest element
-- 1. setup edge condition
-- 2. maximum of a singleton list is equal to the only element in it
-- 3. maximum of a longer list is the head if the head is bigger than maximum
-- 4. if the maximum of the tail is bigger - then its maximum of the tail

maximum' :: (Ord a) => [a] -> a
-- if empty then error
maximum' [] = error "maximum of empty list"
-- if one element than one element is max
maximum' [x] = x
-- splitting a list into head and a tail
maximum' (x:xs)
    | x > maxTail = x
    | otherwise = maxTail
    where maxTail = maximum' xs

-- also possible to write
-- maximum' (x:xs) = max x (maximum' xs)


replicate' :: (Num i, Ord i) => i -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x:replicate' (n-1) x

-- take 3 [5,4,3,2,1] return [5,4,3]

take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _
    | n <= 0    = []
take' _ []      = []
take' n (x:xs)  = x : take' (n-1) xs

-- reverse

reverse' :: [a] -> [a]
reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

-- zip [1,2,3] [2,3] returns [(1,2),(2,3)]

zip' :: [a] -> [b] -> [(a,b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x,y):zip' xs ys

-- quicksort implementation
-- a sorted list is a list that has all the values smaller than (or equal to)
-- the head of the list in front (and those values are sorted),
-- then comes the head of the list in the middle and
-- then come all the values that are bigger than the head (they're also sorted).

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort [a | a <- xs, a <= x]
        biggerSorted = quicksort [a | a <- xs, a > x]
    in  smallerSorted ++ [x] ++ biggerSorted



-- ghci> quicksort [10,2,5,3,1,6,7,4,2,3,4,8,9]
-- [1,2,2,3,3,4,4,5,6,7,8,9,10]

-- [5,1,9,4,6,7,3]
-- take 5
-- [1,4,3] ++ [5] ++ [9,6,7
-- Now, if we sort [1,4,3] and [9,6,7]

-- quicksoft with filter

-- filter (<=x) xs      instead       a | a <- xs, a <= x
-- filter (>x) xs       instead       a | a <- xs, a > x
quicksortFilter :: (Ord a) => [a] -> [a]
quicksortFilter [] = []
quicksortFilter (x:xs) =
    let smallerSorted = quicksortFilter (filter (<=x) xs)
        biggerSorted = quicksortFilter (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted



