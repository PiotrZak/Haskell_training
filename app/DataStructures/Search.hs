module DataStructures.Search where


-- sequence of elements
-- a1 , a2 , a3 , a4 , ... , an

-- how to find 3?

-- scan the list from left to right and add 1 to each element

-- 1                -- is not > 3
-- 1 + 1            -- is not > 3
-- 1 + 1 + 1        -- is not > 3
-- 1 + 1 + 1 + 1    -- is > 3
-- ...

-- how to find element of a least priority?

-- v1                                -- still bigger than v
-- v1 `min` v2                       -- still bigger than v
-- v1 `min` v2 `min` v3              -- still bigger than v
-- v1 `min` v2 `min` v3 `min` v4     -- equal to v!
-- ...

-- In general terms, we are looking for the position where a predicate p switches from False to

-- measure a1                                              -- not p
-- measure a1 <> measure a2                                -- not p
-- measure a1 <> measure a2 <> measure a3                  -- not p
-- measure a1 <> measure a2 <> measure a3 <> measure a4    -- p
-- ...                                                     -- p



-- position of k is true

-- p (measure a1 <> ... <> measure ak)                    is  False
-- p (measure a1 <> ... <> measure ak <> measure a(k+1))  is  True

-- binary search

-- x <> y
--    x =       measure a1 <> ... <> measure a(n/2)
--    y = measure a(n/2+1) <> ... <> measure an


search :: Measured a v => (v -> Bool) -> Tree v a -> Maybe a
search p t
    | p (measure t) = Just (go mempty p t)
    | otherwise     = Nothing
    where
    go i p (Leaf _ a) = a
    go i p (Branch _ l r)
        | p (i <> measure l) = go i p l
        | otherwise          = go ( i <> measure l p r)

-- p (x)  implies  p (x <> y)   for all y

t !! k = search (> k)
winner t = search (== measure t)