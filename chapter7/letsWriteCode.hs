module LetsWriteCode where

  tensDigit :: Integral a => a -> a
  tensDigit x = d
    where
      (xLast, _) = divMod x 10
      (_, d) = divMod xLast 10
      -- xLast = x `div` 10
      -- d = xLast `mod` 10

  hunsD :: Integral a => a -> a
  hunsD x = d
    where
      (xLast, _) = divMod x 100
      (_, d) = divMod xLast 10

  foldBool :: a -> a -> Bool -> a
  foldBool x y bool =
    case bool of
      True -> x
      False -> y

  foldBoolGuard :: a -> a -> Bool -> a
  foldBoolGuard x y bool
    | bool == False = y
    | bool == True = x

  g :: (a -> b) -> (a, c) -> (b, c)
  g fn (a, c) = (fn a, c)
