module DataStructures.PriorityQueue where
import DataStructures.BinaryTree

type Priority = Int

--      2
--    /   \
--   4     2
--  / \   / \
-- 16  4  2  8
-- a   a  a / \
--         32  8
--         a   a

-- tag (Leaf .. a)     = priority a
-- tag (Branch .. x y) = tag x `min` tag y

-- winner :: Tree Priority a -> a
-- winner t = go t
--     where
--    go (Leaf _ a)         = a
--    go (Branch _ x y)
--        | tag x == tag t = go x     -- winner on left
--        | tag y == tag t = go y     -- winner on right

