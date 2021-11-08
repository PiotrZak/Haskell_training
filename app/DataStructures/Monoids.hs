module DataStructures.Monoids where


-- tag (Branch .. x y) = tag x <> tag y

instance Monoid Size where
    mempty = 0
    mappend = (+)

instance Monoid Priority where
    mempty = maxBound
    mappend = min

branch :: Monoid v => Tree v a -> Tree v a -> Tree v a
branch x y = Branch (x <> y) x y

class Monoid v => Measured a v where
    measure :: a -> v

leaf :: Measured a v => a -> Tree v a
leaf a = Leaf (measure a) a

instance Monoid v => Measured a v where
    measure :: a -> v -- one element = size 1

instance Measured Foo Priority where
    measure a = priority a -- urgency of the element

--

-- (v1<>v2) <> (v3<>v4)         v1 <> (v2<>(v3<>v4))
--        /    \                  /  \
--       /      \               v1   v2 <> (v3<>v4)
--      /        \              a1     /  \
--  v1 <> v2  v3 <> v4               v2   v3 <> v4
--    /  \      /  \                 a2     /  \
--   v1  v2    v3  v4                     v3   v4
--   a1  a2    a3  a4                     a3   a4