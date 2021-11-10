module Modules where
--import Data.List

-- importing selected elements
import Data.List (nub, sort)

-- importing elements excepts particular
-- import Data.List hiding (nub)

--
-- import qualified Data.Map

-- also as M e.g.


numUniques :: (Eq a) => [a] -> Int
numUniques = length . nub


-- Data.List

intersperse 0 [1,2,3,4,5,6] -- r: [1,0,2,0,3,0,4,0,5,0,6]
intercalate [0,0,0] [[1,2,3],[4,5,6],[7,8,9]]  -- r: [1,2,3,0,0,0,4,5,6,0,0,0,7,8,9]
transpose [[1,2,3],[4,5,6],[7,8,9]] -- 2D Matrix - columns become rows and vice versa r: [[1,4,7],[2,5,8],[3,6,9]]

-- adding those polynomials
-- 3x2 + 5x + 9, 10x3 + 9
-- 8x3 + 5x2 + x - 1

-- those list representation in haskell: [0,3,5,9], [10,0,0,9] and [8,5,1,-1]

map sum $ transpose [[0,3,5,9],[10,0,0,9],[8,5,1,-1]] -- r: [18,8,6,17]

-- flatten list -> concat

concat ["foo", "bar", "car"] -- r: "foobarcar"
concat [[3,4,5], [2,3,4], [2,1,1]] -- r: "[3,4,5,2,3,4,2,1,1]


-- Doing concatMap is the same as first mapping a function to a list and then concatenating the list with concat.

concatMap (replicate 4) [1..3] -- r: [1,1,1,1,2,2,2,2,3,3,3,3]

-- and takes a list of boolean values and returns True only if all the values in the list are True.

and $ map (>4) [5,6,7,8] -- r: false
and $ map (==4) [4,4,4,3,4] -- r: false

-- or is like and, only it returns True if any of the boolean values in a list is True.
or $ map (==4) [2,3,4,5,6,1] -- r: true
or $ map (>4) [1,2,3] -- r: false

-- any and all take a predicate and then check if any or all the elements in a list satisfy the predicate, respectively. Usually we use these two functions instead of mapping over a list and then doing and or or.

any (==4) [2,3,5,6,1,4] -- r: true
all (>4) [6,9,10] -- r: true
all (`elem` ['A'..'Z']) "HEYGUYSwhatsup" -- r: false
any (`elem` ['A'..'Z']) "HEYGUYSwhatsup" --  r: true

--iterate takes a function and a starting value. It applies the function to the starting value, then it applies that function to the result, then it applies the function to that result again, etc. It returns all the results in the form of an infinite list.

take 10 $ iterrate (*2) 1 -- r: [1,2,4,8,16,32,64,128,256,512]
take 3 $ iterate (++ "haha") "haha"  -- r: ["haha","hahahaha","hahahahahaha"]

-- splitAt takes a number and a list. It then splits the list at that many elements, returning the resulting two lists in a tuple.

splitAt 3 "heyman" -- ("hey","man")
splitAt 100 "heyman" -- ("heyman","")
splitAt (-3) "heyman" -- ("","heyman")
let (a,b) = splitAt 3 "foobar" in b ++ a  -- "barfoo"

-- takeWhile is a really useful little function. It takes elements from a list while the predicate holds and then when an element is encountered that doesn't satisfy the predicate, it's cut off. It turns out this is very useful.

takeWhile (>3) [6,5,4,3,2,1,2,3,4,5,4,3,2,1] -- r: [6,5,4]
takeWhile (/=' ') "This is a sentence"  -- r: "This"

-- the sum of all third powers that are under 10,000.
sum $ takeWhile(<10000) $ map (^3) [1..] -- r: 53361

dropWhile (<3) [1,2,2,2,3,4,5,4,3,2,1]  -- r: [3,4,5,4,2,1]

let stock = [(994.4,2008,9,1),(995.2,2008,9,2),(999.2,2008,9,3),(1001.4,2008,9,4),(998.3,2008,9,5)]
head (dropWhile (\(val,y,m,d) -> val < 1000) stock) -- r: head (dropWhile (\(val,y,m,d) -> val < 1000) stock)


let (fw, rest) = span (/=' ') "This is a sentence" in "First word:" ++ fw ++ ", the rest:" ++ rest
"First word: This, the rest: is a sentence"

-- Whereas span spans the list while the predicate is true, break breaks it when the predicate is first true. Doing break p is the equivalent of doing span (not . p).

break (==4) [1,2,3,4,5,6,7] -- r: ([1,2,3],[4,5,6,7])
span (/=4) [1,2,3,4,5,6,7] -- r: ([1,2,3],[4,5,6,7])

sort [8,5,3,2,1,6,4,2]  -- r: [1,2,2,3,4,5,6,8]

group [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]  -- r: [[1,1,1,1],[2,2,2,2],[3,3],[2,2,2],[5],[6],[7]]

map (\l@(x:xs) -> (x,length l)) . group . sort $ [1,1,1,1,2,2,2,2,3,3,2,2,2,5,6,7]
[(1,4),(2,7),(3,2),(5,1),(6,1),(7,1)]

-- First we call tails with the list in which we're searching. Then we go over each tail and see if it starts with what we're looking for.

-- inits and tails
-- are like init and tail, only they recursively apply that to a list until there's nothing left.

-- ghci> inits "w00t"
-- ["","w","w0","w00","w00t"]
-- ghci> tails "w00t"
-- ["w00t","00t","0t","t",""]

search :: (Eq a) => [a] -> [a] -> Bool
search nedle haystack =
    let nlen = length needle
    in foldl (\acc x -> if take nlen x == needle then True else acc) False (tails haystack)

-- partition takes a list and a predicate and returns a pair of lists.
-- The first list in the result contains all the elements that satisfy the predicate,
-- the second contains all the ones that don't.

--partition divide by predicate

partition (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy"  -- r: ("BOBMORGAN","sidneyeddy")
partition (>3) [1,3,5,6,3,2,1,0,3,7] -- r: ([5,6,7],[1,3,3,2,1,0,3])


span (`elem` ['A'..'Z']) "BOBsidneyMORGANeddy" -- r: ("BOB","sidneyMORGANeddy")

-- While span and break are done once they encounter the first element that doesn't and does satisfy the predicate,
-- partition goes through the whole list and splits it up according to the predicate.

-- find takes a list and a predicate and returns the first element that satisfies the predicate.

find (>4) [1,2,3,4,5,6] -- r: Just 5
find (>9) [1,2,3,4,5,6] -- r: Nothing

-- Maybe can contain either no elements or one element
-- A list can contain no elements, one element or several elements.
find :: (a -> Bool) -> [a] -> Maybe a

-- findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate.

findIndex (==4) [5,3,2,1,6,4]  -- r: Just 5
findIndices (`elem` ['A'..'Z']) "Where Are The Caps?"  -- r: [0,6,10,14]


-- zip connect list together ->

zipWith3 (\x y z -> x + y + z) [1,2,3] [4,5,2,2] [2,2,3] -- r: [7,9,8]
zip4 [2,3,3] [2,2,2] [5,5,3] [2,2,2]  -- r: [(2,2,5,2),(3,2,5,2),(3,2,3,2)]


lines "first line\nsecond line\nthird line"  -- r: ["first line","second line","third line"]
-- unlines is reverse of previous process


-- union also acts like a function on sets. It returns the union of two lists.

[1..7] `union` [5..10] -- r: [1,2,3,4,5,6,7,8,9,10] - concat not duplicates values

-- intersect works like set intersection. It returns only the elements that are found in both lists.
[1..7] `intersect` [5..10] -- r: [5,6,7] - common part

insert 4 [3,5,1,2,8,2]  -- r: [3,4,5,1,2,8,2]
insert 4 [1,3,4,4,1] -- r: [1,3,4,4,4,1]

-- genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate
-- The nub, delete, union, intersect and group functions all have their more general counterparts called nubBy, deleteBy, unionBy, intersectBy and groupBy
-- group is the same as groupBy (==)

let values = [-4.3, -2.4, -1.2, 0.4, 2.3, 5.9, 10.5, 29.1, 5.3, -2.4, -14.5, 2.9, 2.3]

-- The equality function supplied takes two elements returns True only if they're both negative or if they're both positive.
groupBy (\x y -> (x > 0) == (y > 0)) values


on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on` g = \x y -> f (g x) (g y)

-- (==) `on` (> 0) returns an equality function that look like \x y -> (x > 0) == (y > 0)

groupBy ((==) `on` (> 0)) values
[[-4.3,-2.4,-1.2],[0.4,2.3,5.9,10.5,29.1,5.3],[-2.4,-14.5],[2.9,2.3]]

-- sort, insert, maximum and minimum
-- sortBy, insertBy, maximumBy and minimumBy


let xs = [[5,4,5,4,4],[1,2,3],[3,5,4,3],[],[2],[2,2]]
sortBy (compare `on` length) xs
-- r: [[],[2],[2,2],[1,2,3],[3,5,4,3],[5,4,5,4,4]]


-- Data.Char
filter (not . any isSpace) . groupBy ((==) `on` isSpace) $ "hey test test test" -- r: ["hey", "test", "test", "test"]

-- generalCategory :: Char -> GeneralCategory

generalCategory ' '  -- r: Space
generalCategory 'A' -- r: UppercaseLetter
generalCategory '9' -- r: DecimalNumber

map generalCategory " \t\nA9?|" -- r: [Space,Control,Control,UppercaseLetter,DecimalNumber,OtherPunctuation,MathSymbol]
map digitToInt "34538" -- r: [3,4,5,3,8]
map digitToInt "FF86AB" -- r: [15,15,8,5,10,11]

-- intToDigit - is a reverse

encode :: Int -> String -> String
encode shift msg =
    let ords = map ord msg
        shifted = map (+ shift) ords
    in map chr shifted

decode :: Int -> String -> String
decode shift msg = encode (negate shift) msg


-- take a key and list
-- filters the list so that only matching keys remains
-- gets the first key-value that matches and returns the value

-- runtime error when not found
findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key xs = snd . head . filter (\(k,v) -> key == k) $ xs


findKeyError :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyError key [] = Nothing
findKeyError key ((k,v):xs) = if key == k
                            then Just v
                            else findKey key xs


-- after refactoring
findKeyAcc :: (Eq k) => k -> [(k,v)] -> Maybe v
findKeyAcc key = foldr (\(k,v) acc -> if key == k then Just v else acc) Nothing


--array of objects:
--phoneBook =[("betty","555-2938"),("bonnie","452-2928"),("patsy","493-2928"),("lucille","205-2928"),("wendy","939-8282"),("penny","853-2492")]

findKey "bonnie" phoneBook -- r: Just "452-2928"


-- Data.Map
import qualified Data.Map as Map


-- insert takes a key, a value and a map and returns a new map that's just like the old one, only with the key and value inserted


fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr(\(k,v) acc -> Map.insert k v acc) Map.empty

Map.null Map.empty  -- r: True
Map.null $ Map.fromList [(2,3),(5,5)]   -- r: False

-- size reports the size of a map.

Map.size Map.empty -- r:0
Map.size $ Map.fromList [(2,4),(3,3),(4,2),(5,4),(6,4)]  -- r: 5

-- singleton takes a key and a value and creates a map that has exactly one mapping.

Map.singleton 3 9 -- r:  fromList [(3,9)]
Map.insert 5 9 $ Map.singleton 3 9 -- r : fromList[(3,9),(5,9)]

-- map and filter work much like their list equivalents.
Map.map (*100) $ Map.fromList [(1,1),(2,4),(3,9)]   -- r: [(1,100),(2,400),(3,900)]
Map.filter isUpper $ Map.fromList [(1,'a'),(2,'A'),(3,'b'),(4,'B')]   -- r: fromList [(2,'A'),(4,'B')]


-- lookup works like the Data.List lookup, only it operates on maps. It returns Just something if it finds something for the key and Nothing if it doesn't

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap xs = Map.fromListWith (\number1 number2 -> number1 ++ ", " ++ number2) xs

Map.lookup "patsy" $ phoneBookToMap phoneBook -- r: ["827-9162","943-2929","493-2928"]

-- get the biggest value with specifi
Map.fromListWith max [(2,3),(2,5),(2,100),(3,29),(3,22),(3,11),(4,22),(4,15)]   -- r: [(2,100),(3,29),(4,22)]

-- Data.Set

text1 = "I just had an anime dream. Anime... Reality... Are they so different?"
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- intersection ( both elements in 2 texts)
Set.intersection set1 set2  -- r: fromList " adefhilmnorstuy"

-- difference (which elements are in 1 and vice versa)
Set.difference set1 set2   -- r: fromList ".?AIRj"

Set.difference set2 set1  -- r: fromList "!Tbcgvw"

-- union unique in both
Set.union set1 set2   -- r: fromList " !.?AIRTabcdefghijlmnorstuvwy"

Set.filter odd $ Set.fromList [3,4,5,6,7,2,3,4]   -- r: fromLst [3,5,7]
Set.map (+1) $ Set.fromList [3,4,5,6,7,2,3,4] -- r: fromList [3,4,5,6,7,8]



