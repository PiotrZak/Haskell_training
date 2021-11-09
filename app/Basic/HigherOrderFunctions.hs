module Basic.HigherOrderFunctions where

-- curried functions

-- max :: (Ord a) => a -> a -> a
-- max :: (Ord a) => a -> (a -> a)

-- takes an a - and returns a functions, that takes a and returns a

-- takes a a a and return a
multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z

-- multThree 3 5 9 or ((multThree 3) 5) 9
-- 3 * 5 * 9 = 15 * 9 = 135
-- multThree :: (Num a) => a -> (a -> (a -> a))

compareWithHundred :: (Num a, Ord a) => a -> Ordering
compareWithHundred x = compare 100 x

-- infix functions

divideByTen :: (Floating a) => a -> a
divideByTen = (/10)

-- Calling, say, divideByTen 200
-- is equivalent to doing 200 / 10,
-- as is doing (/10) 200

isUpperAlphaNum :: Char -> Bool
isUpperAlphaNum = (`elem` ['A'..'Z'])


-- the first parameter (a -> a) is a function
-- the second parameter is a

applyTwice :: (a -> a) -> a -> a
-- parameter f as function - applying x - and then applying the result of f again
applyTwice f x = f (f x)

-- applyTwice (multThree 2 2) 9
-- 144


-- The first parameter is a function that takes two things and produces a third thing
-- The second and third parameter are lists
-- The result is a list

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

-- zipWith' (+) [4,2,5,6] [2,6,2,3] -- it's add and zipping
-- [6,8,7,9]

-- zipWith' max [6,3,2,1] [7,3,1,5]
-- [7,3,2,5]

-- zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]
-- [[3,4,6],[9,20,30],[10,12,12]]


flip' :: (a -> b -> c) -> b -> a -> c
flip' f y z = f x y

--  flip' zip [1,2,3,4,5] "hello"
--  [('h',1),('e',2),('l',3),('l',4),('o',5)]


-- take a function that takes a and return b
-- a list of a's and returns list of b

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (x:xs) : f x : map f xs

--map (+3) [1,5,3,1,6]  r: [4,8,6,4,9]
-- map (map (^2)) [[1,2],[3,4,5,6],[7,8]]   r: [[1,4],[9,16,25,36],[49,64]]

-- f p x evaluates to True, the element gets included in the new list.
-- If it doesn't, it stays out.

filter :: (a -> Bool) -> [a] -> [a]
filter _ [] = []
filter p (x:xs)
    | p x       = x : filter p xs
    | otherwise = filter p xs

-- filter (>3) [1,5,3,2,1,6,4,3,2,1]  r: [5,6,4]
-- filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"  r" "uagameasadifeent"
-- filter even [1..10] r: [2,4,6,8,10]

-- pure functional language

-- find the largest number under 100,000 that's divisible by 3829.
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000,99999..])
    where p x = x `mod` 3829 == 0

-- We first make a list of all numbers lower than 100,000,
-- descending. Then we filter it by our predicate and
-- because the numbers are sorted in a descending manner,
-- the largest number that satisfies our predicate is the
-- first element of the filtered list.
-- We didn't even need to use a finite list for our starting set.
-- That's laziness in action again.
-- Because we only end up using the head of the filtered list,
-- it doesn't matter if the filtered list is finite or infinite.
-- The evaluation stops when the first adequate solution is found.


-- to find the sum of all odd squares that are smaller than 10,000.

-- takeWhile:
-- It takes a predicate and a list and then goes from
-- the beginning of the list and returns its elements
-- while the predicate holds true

-- It's a matter of taste as to which one you find prettier
sum (takeWhile (<10000) (filter odd (map (^2) [1..])))
-- 166650

sum (takeWhile (<10000) [n^2 | n <- [1..], odd (n^2)])
-- 166650


-- Collatz sequences

-- take a number
-- if even - then divide by 2
-- if odd - *3
-- add 1
-- result: chain of numbers

-- if 13 then -> 13, 40, 20, 10, 5, 16, 8, 4, 2, 1.
 -- 13*3 + 1 -> 40
 -- 40 /2 -> 20
 -- 20 / 2 -> 10
 -- 10 / 2 -> 5
 -- 5*3 +1 -> 16
 -- (...)

 chain :: (Integral a) => a -> [a]
 chain 1 = [1]
 chain n
    | even n =  n:chain (n `div` 2)
    | odd n  =  n:chain (n*3 + 1)

 -- chain 30  r : [30,15,46,23,70,35,106,53,160,80,40,20,10,5,16,8,4,2,1]

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1...100]))
    where isLong xs = length xs > 15

-- possible with curring -> map (*) [0..] or map (*2) [0..]


-- Lambdas
-- basically anonymous functions

-- \ - symbol of lambda

numLongChains :: Int
numLongChains = length (filter(\xs -> length xs > 15) (map chain [1...100]))

-- map (+3) [1,6,3,2] and map (\x -> x + 3) [1,6,3,2] is equivalent
-- since both (+3) and (\x -> x +3) are functions

zipWith (\a b -> (a * 30 + 3) / b) [5,4,3,2,1] [1,2,3,4,5]
-- r: [153.0,61.5,31.0,15.75,6.6]

map (\(a,b) -> a + b) [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- r: [3,8,9,8,7]

addThree :: (Num a) => a -> a -> a -> a
addThree x y z = x + y + z

addThree :: (Num a) => a -> a -> a -> a
addThree = \x -> \y -> \z -> x + y + z

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = \x y -> f y x


-- Only Folds and horses


-- foldl

-- folds the list up from the left side.
-- The binary function is applied between the starting value
-- and the head of the list.
-- That produces a new accumulator value
-- and the binary function is called with
-- that value and the next element, etc.

sum' :: (Num a) => [a] -> a
sum' xs = foldl (\acc x -> acc + x) 0 xs

-- sum' [3,5,2,1]  r: 11
-- 0 + 3, 3+5, 8+2, 10+1,

-- \acc x -> acc + x - binary functions staring with xs
-- and the result becomes the new accumulator

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

-- The lambda function (\acc x -> acc + x) is the same as (+)


elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys


-- the right foldr - works in similar way
-- but accumulator eats up the values from right.

map' :: (a -> b) -> [a] -> [b]
map' f xs = foldr (\x acc -> f x : acc) [] xs

--If you reverse a list, you can do a right fold on it just like
-- you would have done a left fold and vice versa.
-- Sometimes you don't even have to do that. The sum function


maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
-- \acc x -> x : acc  reversed list
reverse' = foldl (\acc x -> x : acc) []

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- scanl and scanr are like foldl and foldr,
-- only they report all the intermediate accumulator
-- states in the form of a list.

-- How many elements does it take for the sum of
-- the roots of all natural numbers to exceed 1000?

sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- sqrtSums r: 131
-- sum (map sqrt [1..131])  r: 1005.0942035344083
-- sum (map sqrt [1..130]) r: 993.6486803921487

-- We use takeWhile here instead of filter because filter doesn't work on infinite lists.


-- $ - function application

($) :: (a -> b) -> a -> b
f $ x = f x

-- f a b c is the same as ((f a) b) c)
-- function with $ is right associative

sum (map sqrt [1..130])
sum $ map sqrt [1..130]

-- When a $ is encountered,
-- the expression on its right is applied
-- as the parameter to the function on its left

sqrt (3 + 4 + 9)
sqrt $ 3 + 4 + 9

sum (filter (> 10) (map (*2) [2..10]))
sum $ filter (>10) $ map (*2) [2..10]


-- function composition
-- in math defined as (f . g)(x) = f(g(x))
-- meaning that composing two functions produces a new function that,
-- when called with a parameter, say, x is the equivalent of calling g
-- with the parameter x and then calling the f with that result.

-- in Haskell function composition with the . function

(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)

-- The expression negate . (* 3) returns a function that takes a number, multiplies it by 3 and then negates it.

map (\x -> negate (abs x)) [5,-3,-6,7,-3,2,-19,24]
-- r: [-5,-3,-6,-7,-3,-2,-19,-24]

-- the same with function composition
map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
-- r: [-5,-3,-6,-7,-3,-2,-19,-24]

-- Fabulous! Function composition is right-associative,
-- so we can compose many functions at a time.
-- The expression f (g (z x)) is equivalent to (f . g . z) x.
-- With that in mind, we can turn

map (\xs -> negate (sum (tail xs))) [[1..5],[3..6],[1..7]]
map (negate . sum . tail) [[1..5],[3..6],[1..7]]
-- r: [-14,-15,-27]

oddSquareSum :: Integer
oddSquareSum = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

oddSquareSum :: Integer
oddSquareSum = sum . takeWhile (<10000) . filter odd . map (^2) $ [1..]

oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit