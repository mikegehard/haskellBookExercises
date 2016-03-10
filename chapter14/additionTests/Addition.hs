module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "Addition" $ do
        it "" $ do
            (1 + 1) > 1 `shouldBe` True
        it "" $ do
            2 + 2 `shouldBe` 4
        it "x + 1 is always greater than x" $ do
            property $ \x -> x + 1 > (x :: Int)

    describe "Division" $ do
        it "" $ do
            dividedBy 15 3 `shouldBe` (5, 0)
        it "" $ do
            dividedBy 22 5 `shouldBe` (4, 2)


dividedBy :: Integral a => a -> a -> (a, a)
dividedBy num denom = go num denom 0
    where
        go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)


trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements [False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
    a <- arbitrary
    b <- arbitrary
    return (a, b)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
    a <- arbitrary
    b <- arbitrary
    elements [Left a, Right b]