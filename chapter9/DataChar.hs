module DataChar where
    import Data.Char (isUpper, toUpper)

    onlyCaps :: String -> String
    onlyCaps word = filter isUpper word

    capitalizeFirst :: String -> String
    capitalizeFirst (firstLetter : theRest) = toUpper firstLetter : theRest

    capitalizeAll :: String -> String
    capitalizeAll [] = []
    capitalizeAll (firstLetter : theRest) = toUpper firstLetter : capitalizeAll theRest
