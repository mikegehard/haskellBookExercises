module VarietyPack where

k :: (t, t1) -> t
k (x, y) = x

k1 :: (Integer, Integer)
k1 = k ((4, 1),  10)

k2 :: String
k2 = k ("three", (1 + 2))

k3 :: Integer
k3 = k (3, True)

-- k3 will return 3

f :: (a, b, c) -> (d, e, f) -> ((a, d), (c, f))
f (a, b, c) (d, e, f) = ((a,d), (c,f))
