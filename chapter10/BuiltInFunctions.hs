module BuiltInFunctions where
    myOr :: [Bool] -> Bool
    myOr = foldr (||) False

    myAny :: (a -> Bool) -> [a] -> Bool
    myAny f list = myOr $ map f list

    myElem :: Eq a => a -> [a] -> Bool
--     myElem item = myAny (== item)
    myElem item list = foldr (||) False $ map (== item) list

    myReverse :: [a] -> [a]
    myReverse list = foldl (flip (:)) [] list

    myMap :: (a -> b) -> [a] -> [b]
    myMap f list = foldr ((:) . f) [] list

    myFilter :: (a -> Bool) -> [a] -> [a]
    myFilter f list = foldr (\item acc -> if (f item) then item : acc else acc) [] list

