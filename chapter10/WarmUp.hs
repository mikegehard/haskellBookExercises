module WarmUp where

    combinations :: [a] -> [b] -> [(a, b, a)]
--     combinations stops vowels = [(a, b, c) | a <- stops, b <- vowels, c <- stops]
    combinations stops vowels = [(a, b, c) | a <- stops, b <- vowels, c <- stops, a == 'p']


