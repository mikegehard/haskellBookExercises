module Ciphers where
    import Data.Char (ord, chr)

    caesar :: Int -> String -> String
    caesar distanceToShift word = [shiftLetter letter | letter <- word]
        where
            startingPoint = ord 'a'
            totalNumberOfLetters = 26
            newOrdinal letter = (mod (ord letter + distanceToShift - startingPoint) totalNumberOfLetters) + startingPoint
            shiftLetter letter = chr $ newOrdinal letter

    unCaesar :: Int -> String -> String
    unCaesar distanceToShift word = undefined