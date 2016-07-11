module GrabBag where

-- 1. They are all equivalent.

-- 2. Num a => a -> a -> a

-- 3.

addOneIfOdd n = case odd n of
    True -> f n
    False -> n
    -- where f n = n + 1
    where f = \n -> n + 1

-- addFive x y = (if x > y then y else x) + 5
addFive = \x -> \y -> (if x > y then y else x) + 5

-- mflip f = \x -> \y -> f y x
mflip f x y = f y x
