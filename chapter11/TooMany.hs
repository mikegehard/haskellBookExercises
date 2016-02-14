module TooMany where

    newtype Goats = Goats Int deriving (Eq, Show)
    instance TooMany Goats where
        tooMany (Goats n) = n > 43

    newtype Cows = Cows Int deriving (Eq, Show)

    class TooMany a where
        tooMany :: a -> Bool

    instance TooMany Int where
        tooMany n = n > 42

