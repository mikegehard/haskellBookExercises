f :: a -> a -> a -> a
f x1 x2 x3 = x1

g :: a -> b -> c -> b
g x1 x2 x3 = x2

h :: (Num a, Num b) => a -> b -> b
h x1 x2 = x2

jackal :: (Ord a, Eq b) => a -> b -> a
jackal x1 x2 = x1

kessel :: (Ord a, Num b) => a -> b -> a
kessel x1 x2 = x1
