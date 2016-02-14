module MyList where

    myOr :: [Bool] -> Bool
    myOr [] = False
    myOr (x : xs) = x || myOr xs

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny _ [] = False
    myAny f (x : xs) = f x || myAny f xs

    myElem :: Eq a => a -> [a] -> Bool
--     myElem _ [] = False
--     myElem toMatch (x : xs) = (toMatch == x) || myElem toMatch xs
    myElem toMatch xs = myAny (== toMatch) xs

    myReverse :: [a] -> [a]
    myReverse [] = []
    myReverse (x : xs) = myReverse xs ++ [x]

    squish :: [[a]] -> [a]
    squish [] = []
    squish (x : xs) = x ++ squish xs

    squishMap :: (a -> [b]) -> [a] -> [b]
    squishMap _ [] = []
    squishMap f (x : xs) = f x ++ squishMap f xs

    squishAgain :: [[a]] -> [a]
    squishAgain xs = squishMap id xs

    myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
    myMaximumBy f (x:xs) = go f xs x
        where
            go _ [] largest = largest
            go f (first : rest) largest =
                if (f largest first) == GT then go f rest largest
                else go f rest first

    myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
    myMinimumBy f (x:xs) = go f xs x
        where
            go _ [] smallest = smallest
            go f (first : rest) smallest =
                if (f smallest first) == LT then go f rest smallest
                else go f rest first

    myMaximum :: (Ord a) => [a] -> a
    myMaximum xs = myMaximumBy compare xs

    myMinimum :: (Ord a) => [a] -> a
    myMinimum xs = myMinimumBy compare xs