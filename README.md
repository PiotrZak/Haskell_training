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