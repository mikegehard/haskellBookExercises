module EnumFromTo where

  myEnumFromTo :: (Enum a, Eq a) => a -> a -> [a]
  myEnumFromTo from to = go from to []
    where
      go from to acc
        | from == (succ to) = []
        | otherwise = from : (go (succ from) to acc)

