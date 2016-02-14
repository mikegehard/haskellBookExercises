module Database where

    import Data.Time
    import Data.Maybe

    data DatabaseItem = DbString String | DbNumber Integer |DbDate UTCTime deriving (Eq, Ord, Show)

    theDatabase :: [DatabaseItem]
    theDatabase =
        [ DbDate (UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123))
        , DbString "Hello, world!"
        , DbDate (UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123))
        , DbNumber 10
        , DbNumber 5
        ]

    filterDbDate :: [DatabaseItem] -> [UTCTime]
    filterDbDate items = catMaybes $ fmap getDate items

    getDate :: DatabaseItem -> Maybe UTCTime
    getDate (DbDate t) = Just t
    getDate _ = Nothing

    filterDbNumber :: [DatabaseItem] -> [Integer]
    filterDbNumber items = catMaybes $ fmap getNumber items

    getNumber :: DatabaseItem -> Maybe Integer
    getNumber (DbNumber t) = Just t
    getNumber _ = Nothing

    mostRecent :: [DatabaseItem] -> UTCTime
    mostRecent items = maximum $ filterDbDate items

    sumDb :: [DatabaseItem] -> Integer
    sumDb items = sum $ filterDbNumber items

    avgDb :: [DatabaseItem] -> Double
    avgDb items = (realToFrac (sumDb items) / (realToFrac (length $ filterDbNumber items))


