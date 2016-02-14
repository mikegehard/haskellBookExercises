module DoesItTypeCheck where

  data Person = Person Bool

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  instance Show Person where
    show (Person b) = "Person " ++ show b

  data Mood =
    Blah
    | Woot deriving Show

  settleDown :: Mood -> Mood
  settleDown x =
    if x == Woot
    then Blah
    else x

  instance Eq Mood where
    (==) Blah Blah = True
    (==) Woot Woot = True
    (==) _ _ = False

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

  s1 = Sentence "dogs" "drool"
  s2 = Sentence "Julie" "loves" "dogs"

  data Rocks =
    Rocks String deriving (Eq, Show)
  data Yeah =
    Yeah Bool deriving (Eq, Show)
  data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

  i :: Num a => a
  i = 1

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk fn a b = (fn a) == b

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith fn int a = (fn a)
