module Jammin where
    import Data.List
    import Data.Ord
    import Data.Function

    data Fruit =
        Peach
        | Plum
        | Apple
        | Blackberry deriving (Eq, Show, Ord)

--     data JamJars = Jam Fruit Int deriving (Eq, Show)

    data JamJars =
        Jam { fruit :: Fruit
            , count :: Int }
            deriving (Eq, Show, Ord)

    row1 = Jam Peach 2
    row2 = Jam Plum 10
    row3 = Jam Apple 13
    row4 = Jam Plum 1
    row5 = Jam Blackberry 88
    row6 = Jam Apple 85
    allJam = [row1, row2, row3, row4, row5, row6]

    totalJars :: [JamJars] -> Int
    totalJars jars = sum $ map count jars

    mostRow :: [JamJars] -> JamJars
    mostRow jars = head $ reverse $ sortBy (comparing count) jars

    sortByFruit :: [JamJars] -> [JamJars]
    sortByFruit jars = sortBy (comparing fruit) jars

    groupJam :: [JamJars] -> [[JamJars]]
--     groupJam jars = groupBy (\a b -> fruit a == fruit b) $ sortByFruit jars
    groupJam jars = groupBy ((==) `on` fruit) $ sortByFruit jars