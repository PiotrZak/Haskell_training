# Haskell_training


:: - declare + reference

___

(!!) :: [a] -> Int -> a

List index (subscript) operator, starting from 0. It is an instance of the more general genericIndex, which takes an index of any integral type.

Reference: http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-List.html#g:16
___

<> - it's an alias for mappend from Data.Monoid module (smashed two monoidal values together)

ghci> [1,2,3] <> [4,5,6]
[1,2,3,4,5,6]

____

=> 

this symbol separated two parts of a type signature:
- On the left, typeclass constraints
- On the right, the actual type

____
->

this symbol is called as function arrow
- it denotes a function that takes an 
- argument of the type on the left 

and returns 
- a value of the type on the right.

____
Pattern matching : 

In a nutshell, patterns are like defining piecewise functions in math. You can specify different function bodies for different arguments using patterns. When you call a function, the appropriate body is chosen by comparing the actual arguments with the various argument patterns.

____
Haskel property of laziness:

Lazy evaluation allows us to write more simple, elegant code than we could in a strict environment.

https://mmhaskell.com/blog/2017/1/16/faster-code-with-laziness

____

-- $ - function application

($) :: (a -> b) -> a -> b  
f $ x = f x  

 the $ function has the lowest precedence. Function application with a space is left-associative (so f a b c is the same as ((f a) b) c)), function application with $ is right-associative.
 
 sum (filter (> 10) (map (*2) [2..10]))
 sum $ filter (>10) $ map (*2) [2..10]