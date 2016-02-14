module ImplementEq where

  data TisAnInteger = TisAn Integer

  instance Eq TisAnInteger where
    (==) (TisAn i) (TisAn i') = i == i'

  data TwoIntegers = Two Integer Integer

  instance Eq TwoIntegers where
    (==) (Two i1 i2) (Two i1' i2') = i1 == i1' && i2 == i2'

  data IntOrString = TisAnInt Int | TisAString String

  instance Eq IntOrString where
    (==) (TisAnInt i) (TisAnInt i') = i == i'
    (==) (TisAString s) (TisAString s') = s == s'
    (==) _ _ = False

  data Pair a = Pair a a

  instance Eq a => Eq (Pair a) where
    (==) (Pair a1 a2) (Pair a1' a2') = a1 == a1' && a2 == a2'

  data Tuple a b = Tuple a b

  instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

  data Which a = ThisOne a | ThatOne a

  instance Eq a => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

  data EitherOr a b = Hello a | Goodbye b

  instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False
