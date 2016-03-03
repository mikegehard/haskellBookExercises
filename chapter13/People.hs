module People where

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show
data PersonInvalid =
    NameEmpty
    | AgeTooLow
    | PersonInvalidUnknown String deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
    | name /= "" && age > 0 = Right $ Person name age | name == "" = Left NameEmpty
    | not (age > 0) = Left AgeTooLow
    | otherwise = Left $ PersonInvalidUnknown $
        "Name was: " ++ show name ++
        " Age was: " ++ show age

displayResult :: Either PersonInvalid Person -> IO ()
displayResult (Right p) = do
    putStrLn ("Yay! Successfully got a person: " ++ show p)
displayResult (Left error) = do
    putStrLn (show error)

gimmePerson :: IO ()
gimmePerson = do
    putStr "Please enter a name: "
    name <- getLine
    putStr "Please enter an age: "
    age <- getLine
    displayResult (mkPerson name (read age))

