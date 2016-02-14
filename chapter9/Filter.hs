module Filter where

    filterMult3 xs = filter (\x -> rem x 3 == 0) xs

    lengthMult3 = length . filterMult3

    myFilter :: String -> [String]
    myFilter sentence =
        let
            wordsToRemove = ["the", "a", "an"]
            isWordToKeep = \word -> not $ elem word wordsToRemove
        in
            filter isWordToKeep (words sentence)
