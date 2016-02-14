module Chapter3Exercises where

addExclamation :: [Char] -> [Char]
addExclamation word = word ++ "!"

findLetter :: Int -> [Char] -> Char
findLetter index word = word !! index

rvrs :: String -> String
rvrs sentence =
  let
    positionOfFirstSpace = 5
    firstWord = take 5 sentence
    theRest = drop 6 sentence
    secondWord = take 2 theRest
    lastWord = drop 3 theRest
  in
    concat [lastWord, " ", secondWord, " ", firstWord]
