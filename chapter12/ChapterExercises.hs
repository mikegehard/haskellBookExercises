module ChapterExercises where

    notThe :: String -> String
    notThe word
        | word == "the" = "a"
        | otherwise = word

    replaceThe :: String -> String
    replaceThe sentence =
        let
            ws = words sentence
            replacedWords = map notThe ws
        in
            unwords replacedWords


    countTheBeforeVowel :: String -> Integer
    countTheBeforeVowel sentence = undefined

    countVowels :: String -> Int
    countVowels sentence =
        let
            vowels = filter isVowel sentence
            isVowel letter = elem letter "aeiou"
        in
            length vowels


    newtype Word' = Word' String deriving (Eq, Show)

    vowels = "aeiou"

    mkWord :: String -> Maybe Word'
    mkWord word =
        if numberVowels > numberConsanants then Nothing
        else Just (Word' word)
        where
            numberVowels  = countVowels word
            numberConsanants = length word - numberVowels


    data Nat =
        Zero
        | Succ Nat
        deriving (Eq, Show)

    natToInteger :: Nat -> Integer
    natToInteger Zero = 0
    natToInteger (Succ nat) = 1 + natToInteger nat

    integerToNat :: Integer -> Maybe Nat
    integerToNat number
        | number < 0 = Nothing
        | otherwise =
            Just (go number)
            where
                go number
                    | number == 0 =
                        Zero
                    | otherwise =
                        Succ (go (number - 1))