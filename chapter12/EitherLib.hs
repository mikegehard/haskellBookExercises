module EitherLib where
    -- lefts' [Left "l", Right "r"]
    -- ["l"]
    lefts' :: [Either a b] -> [a]
--     lefts' eithers = map (\(Left val) -> val ) onlyLefts
    lefts' eithers = foldr (\(Left val) acc -> val : acc) [] onlyLefts
        where
            onlyLefts = filter isLeft eithers


    -- rights' [Left "l", Right "r"]
    -- ["r"]

    rights' :: [Either a b] -> [b]
    rights' eithers = foldr (\(Right val) acc -> val : acc) [] onlyRights
        where
            onlyRights = filter isRight eithers

    isLeft :: Either a b -> Bool
    isLeft (Right _) = False
    isLeft (Left _) = True

    isRight :: Either a b -> Bool
    isRight (Right _) = True
    isRight (Left _) = False

    -- partitionEithers' [Left "l", Right "r"]
    --(["l"], ["r"])
    partitionEithers' :: [Either a b] -> ([a], [b])
    partitionEithers' eithers = (lefts' eithers, rights' eithers)

    -- eitherMaybe' (\x -> x ++ "world") (Left "foo") ==> Nothing
    -- eitherMaybe' (\x -> x ++ "world") (Right "bar") ==> Just "barworld"
    eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
    eitherMaybe' _ (Left _) = Nothing
    eitherMaybe' f (Right val) = Just (f val)