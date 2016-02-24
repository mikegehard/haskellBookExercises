module MaybeLib where
    import Data.Maybe as M (fromJust)

    -- >>> isJust (Just 1)
    -- True
    -- >>> isJust Nothing
    -- False
    isJust :: Maybe a -> Bool
    isJust Nothing = False
    isJust (Just _) = True

    -- >>> isNothing (Just 1)
    -- False
    -- >>> isNothing Nothing
    -- True
    isNothing :: Maybe a -> Bool
    isNothing Nothing = True
    isNothing (Just _) = False

    -- >>> mayybee 0 (+1) Nothing
    --0
    -- >>> mayybee 0 (+1) (Just 1)
    --2
    mayybee :: b -> (a -> b) -> Maybe a -> b
    mayybee initialValue _ Nothing = initialValue
    mayybee _ f (Just number) = f number

    -- >>> fromMaybe 0 Nothing
    --0
    -- >>> fromMaybe 0 (Just 1)
    --1
    fromMaybe :: a -> Maybe a -> a
    fromMaybe def Nothing = def
    fromMaybe _ (Just val) = val

    -- >>> listToMaybe [1, 2, 3]
    -- Just 1
    -- >>> listToMaybe []
    -- Nothing
    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe list = Just (head list)

    -- >>> maybeToList (Just 1)
    -- [1]
    -- >>> maybeToList Nothing
    -- []
    maybeToList :: Maybe a -> [a]
    maybeToList Nothing = []
    maybeToList (Just val) = [val]

    -- >>> catMaybes [Just 1, Nothing, Just 2]
    -- [1, 2]
    -- >>> catMaybes [Nothing, Nothing, Nothing]
    -- []
    catMaybes :: [Maybe a] -> [a]
    catMaybes list = map M.fromJust (filter isJust list)

    -- >>> flipMaybe [Just 1, Just 2, Just 3]
    -- Just [1, 2, 3]
    -- >>> flipMaybe [Just 1, Nothing, Just 3]
    -- Nothing
    flipMaybe :: [Maybe a] -> Maybe [a]
    flipMaybe list =
        if difference > 0 then Nothing
        else Just justValues
        where
            justValues = catMaybes list
            difference = length list - length justValues