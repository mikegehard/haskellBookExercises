module PoemLines where
    import Data.Char (isLetter, isSpace, isSeparator)

    firstSen = "Tyger Tyger, burning bright\n"
    secondSen = "In the forests of the night\n"
    thirdSen = "What immortal hand or eye\n"
    fourthSen = "Could frame thy fearful symmetry?"
    sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen
     -- putStrLn sentences -- should print
     -- Tyger Tyger, burning bright
     -- In the forests of the night
     -- What immortal hand or eye
     -- Could frame thy fearful symmetry?

     -- Implement this
    myLines :: String -> (Char -> Bool) -> [String]
    myLines lines splitter = go lines []
        where
            getFirst :: String -> String
            getFirst sentence = takeWhile splitter sentence
            getRest :: String -> String
            getRest sentence = drop 1 $ dropWhile splitter sentence
            go :: String -> [String] -> [String]
            go sentence acc
                | sentence == [] = []
                | otherwise = (getFirst sentence) : go (getRest sentence) acc

     -- This is what we want 'myLines sentences' to equal
    shouldEqual =
        [ "Tyger Tyger, burning bright"
        , "In the forests of the night"
        , "What immortal hand or eye"
        , "Could frame thy fearful symmetry?" ]
     -- The main function here is a small test
     -- to ensure you've written your function
     -- correctly.
    main :: IO ()
    main =
        print $ "Are they equal? "
        ++ show (myLines sentences (/= '\n') == shouldEqual)

