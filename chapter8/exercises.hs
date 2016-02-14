module Chapter8Exercises where

  cattyConny :: String -> String -> String
  cattyConny x y = x ++ "mrow" ++ y

  flippy :: String -> String -> String
  flippy = flip cattyConny

  appedCatty :: String -> String
  appedCatty = cattyConny "woops"

  frappe :: String -> String
  frappe = flippy "haha"

  sumIt :: (Eq a, Num a) => a -> a
  sumIt 0 = 0
  sumIt n = n + sumIt (n - 1)

  multiplyIt :: (Integral a) => a -> a -> a
  multiplyIt a b = go a b 0
    where
      go a b sum
        | b == 1 = sum + a
        | otherwise = go a (b - 1) (sum + a)


  data DividedResult = Result Integer | DividedByZero deriving (Show)

  dividedBy :: Integral a => a -> a -> DividedResult
  dividedBy num denom
    | denom == 0 =
      DividedByZero
    | num < 0 =
      let
        result = go (-num) denom 0
      in
        Result (negate . fst $ result)
    | denom < 0 =
      let
        result = go num (-denom) 0
      in
        Result (negate . fst $ result)
    | otherwise =
      Result (fst $ go num denom 0)
    where
      go n d count
        | n < d = (count, n)
        | otherwise = go (n - d) d (count + 1)

  -- Num because of use of -
  -- Ord because of use of <=
  mc91 :: (Num a, Ord a) => a -> a
  mc91 num
    | num <= 100 = 91
    | otherwise = num - 10
