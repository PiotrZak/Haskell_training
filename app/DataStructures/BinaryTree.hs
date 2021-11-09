module DataStructures.BinaryTree where
import Prelude hiding (head, (!!))

-- reduce O(n) to O(log n)
-- that type of tree:
--
--
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
tag (Leaf v _)     = v
tag (Branch v _ _) = v

-- retrieves the leftmost element
head :: Tree v a -> a
head (Leaf _ a)     = a
head (Branch _ x _) = x


type Size = Int

tag (Leaf  ..)       = 1
tag (Branch .. x y)  = tag x + tag y

leaf :: a => Tree Size a
leaf a = Leaf 1 a

branch :: Tree Size a -> Tree Size a -> Tree Size a
branch x y = Branch(tag x + tag y) x y

(!!) :: Tree Size a -> Int -> a
(Leaf _ a)      !! 0 = a
(Branch _ x y)  !! n
    | n < tag x      = x !! n
    | otherwise      = y !! (n - tag x)

