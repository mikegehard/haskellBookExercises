funcIgnoreArgs :: a -> a -> a -> String
funcIgnoreArgs x y z = "Blah"

nonsense :: Bool -> Integer
nonsense True = 805
nonsense False = 31337

typicalCurriedFunction :: Integer -> Bool -> Integer
typicalCurriedFunction i b = i + (nonsense b)

anonymous :: Integer -> Bool -> Integer
anonymous = \i b -> i + (nonsense b)

anonymousManuallyNested :: Integer -> Bool -> Integer
anonymousManuallyNested = \i -> \b -> i + (nonsense b)
