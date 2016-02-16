module Trees where

    data BinaryTree a =
        Leaf
        | Node (BinaryTree a) a (BinaryTree a)
        deriving (Eq, Ord, Show)


    mapTree :: (a -> b) -> BinaryTree a -> BinaryTree b
    mapTree _ Leaf = Leaf
    mapTree f (Node left a right) =
        Node (mapTree f left) (f a) (mapTree f right)

    testTree' :: BinaryTree Integer
    testTree' =
        Node (Node Leaf 3 Leaf) 1 (Node Leaf 4 Leaf)

    mapExpected =
        Node (Node Leaf 4 Leaf) 2 (Node Leaf 5 Leaf)

    -- acceptance test for mapTree
    mapOkay =
        if mapTree (+1) testTree' == mapExpected then putStrLn "Map okay!"
        else putStrLn "test failed!"

    preorder :: BinaryTree a -> [a]
    preorder Leaf = []
    preorder (Node left val right) = val : (preorder left ++ preorder right)

    inorder :: BinaryTree a -> [a]
    inorder Leaf = []
    inorder (Node left val right) = preorder left ++ [val] ++ preorder right

    postorder :: Ord a => BinaryTree a -> [a]
    postorder Leaf = []
    postorder (Node left val right) = preorder left ++ preorder right ++ [val]

    foldTree :: (a -> b -> b -> b) -> b -> BinaryTree a -> b
    foldTree = undefined

    testTree :: BinaryTree Integer
    testTree = Node (Node Leaf 1 Leaf) 2 (Node Leaf 3 Leaf)
    -- 1 -- 2 -- 3

    testPreorder :: IO ()
    testPreorder =
        if preorder testTree == [2, 1, 3] then putStrLn "Preorder fine!"
        else putStrLn "Bad news bears."

    testInorder :: IO ()
    testInorder =
        if inorder testTree == [1, 2, 3] then putStrLn "Inorder fine!"
        else putStrLn "Bad news bears."

    testPostorder :: IO ()
    testPostorder =
        if postorder testTree == [1, 3, 2] then putStrLn "Postorder fine!"
        else putStrLn "postorder failed check"

    testFoldTree :: IO ()
    testFoldTree =
        if foldTree (+) 0 testTree == 6 then putStrLn "Foldr fine!"
        else putStrLn "Bad news bears."

    main :: IO ()
    main = do
      mapOkay
      testPreorder
      testInorder
      testPostorder

