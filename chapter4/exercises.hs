module Chapter4Exercises where

  isPalindrome :: (Eq a) => [a] -> Bool
  isPalindrome word = word == reverse word

  myAbs :: Integer -> Integer
  myAbs number = if number > 0 then number else negate number

  f :: (a,b) -> (c,d) -> ((b,d),(a,c))
  f first second = ((snd first, snd second), (fst first, fst second))
