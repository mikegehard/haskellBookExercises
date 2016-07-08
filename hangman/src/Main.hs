module Main where

import           Control.Monad (forever)
import           Data.Char     (toLower)
import           Data.List     (intersperse, nub)
import           Data.Maybe    (isJust)
import           System.Exit   (exitSuccess)
import           System.Random (randomRIO)

main :: IO ()
main = do
    word <- randomWord'
    let puzzle = freshPuzzle (fmap toLower word)
    runGame puzzle

type WordList = [String]

minWordLength :: Int
minWordLength = 5

maxWordLength :: Int
maxWordLength = 9

allWords :: IO WordList
allWords = do
    dict <- readFile "data/dict.txt"
    return (lines dict)

gameWords :: IO WordList
gameWords = do
    words <- allWords
    return (filter gameLength words)
    where
        gameLength word =
            let l = length word
            in l > minWordLength && l < maxWordLength

randomWord :: WordList -> IO String
randomWord words = do
    randomIndex <- randomRIO (0, length words)
    return $ words !! randomIndex

randomWord' :: IO String
randomWord' = gameWords >>= randomWord

data Puzzle = Puzzle String [Maybe Char] [Char]
instance Show Puzzle where
    show (Puzzle _ discovered guessed) =
        (intersperse ' ' $ fmap renderPuzzleChar discovered)
        ++ " Guessed so far: "
        ++ guessed

freshPuzzle :: String -> Puzzle
freshPuzzle word = Puzzle word (map (const Nothing) word) []

charInWord :: Puzzle -> Char -> Bool
charInWord (Puzzle word _ _) c =
    c `elem` word

alreadyGuessed :: Puzzle -> Char -> Bool
alreadyGuessed (Puzzle _ _ guessed) c =
    c `elem` guessed

renderPuzzleChar :: Maybe Char -> Char
renderPuzzleChar Nothing = '_'
renderPuzzleChar (Just c) = c

fillInCharacter :: Puzzle -> Char -> Puzzle
fillInCharacter (Puzzle word filledInSoFar guessed) c =
    Puzzle word newFilledInSoFar (c : guessed)
    where
        zipper :: Char -> Char -> Maybe Char -> Maybe Char
        zipper guessed wordChar guessChar =
            if wordChar == guessed
            then Just wordChar
            else guessChar
        newFilledInSoFar =
            zipWith (zipper c) word filledInSoFar

handleGuess :: Puzzle -> Char -> IO Puzzle
handleGuess puzzle guess = do
    putStrLn $ "Your guess was: " ++ [guess]
    case (charInWord puzzle guess, alreadyGuessed puzzle guess) of
        (_, True) -> do
            putStrLn "You already guessed that letter."
            return puzzle
        (True, _) -> do
            putStrLn "This character was in the word."
            return (fillInCharacter puzzle guess)
        (False, _) -> do
            putStrLn "This character was not in the word."
            putStrLn ""
            return (fillInCharacter puzzle guess)

gameOver :: Puzzle -> IO ()
gameOver p@(Puzzle word _ _) =
    if tooManyGuesses p then
        do
            putStrLn "You lost!"
            putStrLn $ "The word was: " ++ word
            exitSuccess
    else return ()


tooManyGuesses :: Puzzle -> Bool
tooManyGuesses (Puzzle _ filledInSoFar guessed) =
    length guessed - length uniqueFilledIn > 7
    where
        filledIn = filter isJust filledInSoFar
        uniqueFilledIn = nub filledIn


gameWin :: Puzzle -> IO ()
gameWin (Puzzle _ filledInSoFar _) =
    if all isJust filledInSoFar then
        do
            putStrLn "You win!"
            exitSuccess
    else return ()

runGame :: Puzzle -> IO ()
runGame puzzle = forever $ do
    gameOver puzzle
    gameWin puzzle
    putStrLn $ "Current puzzle is: " ++ show puzzle
    putStr "Guess a letter: "
    guess <- getLine
    case guess of
        [c] -> handleGuess puzzle c >>= runGame
        _ -> putStrLn "Your guess must be a single character."

