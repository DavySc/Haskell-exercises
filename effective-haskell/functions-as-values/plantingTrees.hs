module PlantingTrees where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l a r) = leftString <> ", " <> a <> rightString
  where
    leftString = showStringTree l
    rightString = showStringTree r

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree = undefined

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist = undefined
