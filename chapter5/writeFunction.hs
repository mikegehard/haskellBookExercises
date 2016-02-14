module WriteFunction where

  i :: a -> a
  i x = x

  c :: a -> b -> a
  c x y = x

  c'' :: b -> a -> b
  c'' x y = x

  c' :: a -> b -> b
  c' x y = y

  co :: (b -> c) -> (a -> b) -> (a -> c)
  co f1 f2 = f1 . f2

  a :: (a -> c) -> a -> a
  a f x = x

  a' :: (a -> b) -> a -> b
  a' f x = f x
