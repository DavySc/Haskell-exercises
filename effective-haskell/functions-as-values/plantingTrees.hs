module PlantingTrees where

data BinaryTree a = Leaf | Branch (BinaryTree a) a (BinaryTree a)

showStringTree :: BinaryTree String -> String
showStringTree Leaf = ""
showStringTree (Branch l a r) = leftString <> ", " <> a <> rightString
  where
    leftString = showStringTree l
    rightString = showStringTree r

addElementToIntTree :: BinaryTree Int -> Int -> BinaryTree Int
addElementToIntTree tree n = case tree of
  Leaf -> Branch Leaf n Leaf
  Branch l a r
    | n > a -> Branch l a (addElementToIntTree r n)
    | n < a -> Branch (addElementToIntTree l n) a r
    | otherwise -> Branch l a r

doesIntExist :: BinaryTree Int -> Int -> Bool
doesIntExist Leaf _ = False
doesIntExist (Branch l a r) n
  | n < a = doesIntExist l n
  | n > a = doesIntExist r n
  | otherwise = True
