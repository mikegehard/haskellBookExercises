module MyWords where
    import Data.Char (isLetter, isSpace)
    import Debug.Trace (trace)

    myWords :: String -> [String]
    myWords sentence = go sentence []
        where
            getFirst :: String -> String
            getFirst sentence = takeWhile isLetter sentence
            getRest :: String -> String
            getRest sentence = drop 1 $ dropWhile isLetter sentence
            go :: String -> [String] -> [String]
            go sentence acc
                | sentence == [] = []
                | otherwise = (getFirst sentence) : go (getRest sentence) acc