module TaskThree where

import           TreePrinters
                 ( Tree (..)
                 , verticalPrint
                 )

find :: Ord a => Tree a -> a -> Bool
find Leaf _       = False
find (Node x left right) toFind
    | toFind < x  = find left toFind
    | toFind == x = True
    | toFind > x  = find right toFind

insert :: Ord a => Tree a -> a -> Tree a
insert Leaf toInsert = Node toInsert Leaf Leaf
insert (Node x left right) toInsert
    | toInsert < x   = Node x (insert left toInsert) right
    | toInsert == x  = Node toInsert left right
    | toInsert > x   = Node x left $ insert right toInsert

toList :: Ord a => Tree a -> [a]
toList Leaf                = []
toList (Node x left right) = (toList left) ++ [x] ++ (toList right)

fromList :: Ord a => [a] -> Tree a
fromList []       = Leaf
fromList (x : xs) = insert (fromList xs) x

delete :: Ord a => Tree a -> a -> Tree a
delete Leaf _        = Leaf
delete (Node x left right) toDelete
    | toDelete < x   = Node x (delete left toDelete) right
    | toDelete == x  = deleteNow (Node toDelete left right)
    | toDelete > x   = Node x left $ delete right toDelete

deleteNow :: Ord a => Tree a -> Tree a
deleteNow (Node _ left Leaf)  = left
deleteNow (Node _ Leaf right) = right
deleteNow (Node _ left right) = Node (findBiggest left) (delete left $ findBiggest left) right

findBiggest :: Ord a => Tree a -> a
findBiggest (Node x _ Leaf) = x
findBiggest (Node _ left _) = findBiggest left
