module WordNumber where
  import Data.List (intersperse)
  import Data.Char (digitToInt)

  digitToWord :: Int -> String
  digitToWord n
    | n == 1 = "one"
    | n == 2 = "two"
    | n == 3 = "three"
    | n == 4 = "four"
    | n == 5 = "five"
    | n == 6 = "six"
    | n == 7 = "seven"
    | n == 8 = "eight"
    | n == 9 = "nine"
    | otherwise = "does not compute"

  digits :: Int -> [Int]
  digits n = map digitToInt (show n)

  wordNumber :: Int -> String
  wordNumber n =
    let
      numberWords = map digitToWord $ digits n
    in
      concat $ intersperse "-" numberWords
