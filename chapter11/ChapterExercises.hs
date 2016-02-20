module ChapterExercises where
    import Data.Char
    import Data.List (intercalate)

    isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
    isSubsequenceOf [] target = True
    isSubsequenceOf (head : tail) target = elem head target && isSubsequenceOf tail target

    capitalizeWords :: String -> [(String, String)]
    capitalizeWords xs =
        [(word, capitalizeWord word) | word <- words xs]

    capitalizeWord :: String -> String
    capitalizeWord (head : tail) = toUpper head : tail

    data Expr =
        Lit Integer
        | Add Expr Expr

    eval :: Expr -> Integer
    eval (Lit i) = i
    eval (Add e1 e2) = eval e1 + eval e2

    printExpr :: Expr -> String
    printExpr (Lit i) = show i
    printExpr (Add e1 e2) = printExpr e1 ++ " + " ++ printExpr e2