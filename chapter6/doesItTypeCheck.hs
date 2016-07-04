module DoesItTypeCheck where

  import Data.List (sort)

  data Person = Person Bool deriving Show

  printPerson :: Person -> IO ()
  printPerson person = putStrLn (show person)

  -- Don't need this if you derive Show above
  -- instance Show Person where
    -- show (Person b) = "Person " ++ show b

  data Mood =
    Blah
    | Woot deriving (Show, Eq)

  settleDown :: Mood -> Mood
  settleDown x =
    if x == Woot
    then Blah
    else x

  -- Don't need this if you are deriving Eq above
  -- instance Eq Mood where
  --   (==) Blah Blah = True
  --   (==) Woot Woot = True
  --   (==) _ _ = False

  type Subject = String
  type Verb = String
  type Object = String

  data Sentence =
    Sentence Subject Verb Object deriving (Eq, Show)

  s1 :: Object -> Sentence
  s1 = Sentence "dogs" "drool"

  s2 :: Sentence
  s2 = Sentence "Julie" "loves" "dogs"

  data Rocks =
    Rocks String deriving (Eq, Show)
  data Yeah =
    Yeah Bool deriving (Eq, Show)
  data Papu =
    Papu Rocks Yeah deriving (Eq, Show)

  -- Papu data constructor takes a Rocks and Yeah
  -- phew = Papu "chases" True

  truth = Papu (Rocks "chomsydoz") (Yeah True)

  equalityForall :: Papu -> Papu -> Bool
  equalityForall p p' = p == p'

  -- Papu doesn't implement Ord typeclass
  -- comparePapus :: Papu -> Papu -> Bool
  -- comparePapus p p' = p > p'

  i :: Num a => a
  -- Can't use this because GHC can't assign a Num
  -- to the generic a
  -- i :: a
  i = 1

  -- f :: Float
  -- Can't use this because a needs to be a Fractional
  -- f :: Num a => a
  -- This is OK because 1.0 is a Fractional
  -- f :: Fractional a => a
  -- This is OK because RealFrac is a Fractional as well.
  f :: RealFrac a => a
  f = 1.0

  -- freud :: a -> a
  -- OK because it just tightens up the bounds
  -- freud :: Ord a => a -> a
  -- OK because it tightens up the bounds further.
  freud :: Int -> Int
  freud x = x

  myX = 1 :: Int

  sigmund :: Int -> Int
  -- This doesn't work because a can't be polymorphic
  -- because it returns an Int in the form of myX
  -- sigmund :: a -> a
  sigmund x = myX

  sigmund' :: Int -> Int
  -- Doesn't work because Num is still too general.
  -- sigmund' :: Num a => a -> a
  sigmund' x = myX

  -- jung :: Ord a => [a] -> a
  -- This works because Int is more specific than Ord
  jung :: [Int] -> Int
  jung xs = head (sort xs)

  -- young :: [Char] -> Char
  -- This works because you don't really need to be 
  -- as specific as Char. This will work for anything orderable.
  young :: Ord a => [a] -> a
  young xs = head (sort xs)

  mySort :: [Char] -> [Char]
  mySort = sort

  signifier :: [Char] -> Char
  -- This will not work because mySort is already too specific when
  -- it specifies Char
  -- signifier :: Ord a => [a] -> a
  signifier xs = head (mySort xs)

  chk :: Eq b => (a -> b) -> a -> b -> Bool
  chk fn a b = fn a == b

  arith :: Num b => (a -> b) -> Integer -> a -> b
  arith fn int a = fn a
