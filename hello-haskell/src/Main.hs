module Main where

import Hello
import System.IO

main :: IO ()
main = do
    -- If you don't have this then the prompt doesn't
    -- get printed until after you enter your name.
    hSetBuffering stdout NoBuffering
    putStr "Please input your name: "
    name <- getLine
    sayHello name