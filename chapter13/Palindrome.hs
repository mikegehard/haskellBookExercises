module Palindrome where

import Control.Monad
import System.Exit as SE (exitFailure)
import Data.Char (isLetter, toLower)

palindrome :: IO ()
palindrome = forever $ do
    line1 <- getLine
    case (allLowerCaseLetters line1 == (reverse . allLowerCaseLetters) line1) of
        True ->
            putStrLn "It's a palindrome!"
        False -> do
                putStrLn "Nope!"
                SE.exitFailure
    where
        allLowerCaseLetters line = map toLower $ filter isLetter line