module Estruturas.BSTree where
{-
	===============================================
	================= *BSTree* ====================
	===============================================
-}

data BSTree a = BSTree_Empty 
                | Leaf a (BSTree a) (BSTree a)
				| BSTree_Element_Exists
                deriving (Show, Read)

			   
getElement :: Ord(a) => BSTree a -> a -> Maybe a
getElement BSTree_Empty _ = Nothing
getElement (Leaf a left right) element
                  |element == a = Just a
                  |element > a = getElement left element
                  |element < a = getElement right element

addElement :: Ord(a) => BSTree a -> a -> BSTree a
addElement BSTree_Empty a = Leaf a BSTree_Empty BSTree_Empty
addElement tree@(Leaf current left right ) new 
		| current == new = BSTree_Element_Exists
        | current > new = Leaf current (addElement left new) right
        | current < new = Leaf current left (addElement right new)

replaceLeaf :: Ord(a) => BSTree a -> BSTree a -> BSTree a
replaceLeaf BSTree_Empty a = a
replaceLeaf a BSTree_Empty = a
replaceLeaf tree@(Leaf current left right ) newTree@(Leaf new nLeft nRight) 
        | current == new = newTree
        | current > new = Leaf current (addElement left new) right
        | current < new = Leaf current left (addElement right new)
        
replaceElement :: Ord(a) => BSTree a -> a -> BSTree a
replaceElement BSTree_Empty a = Leaf a BSTree_Empty BSTree_Empty
replaceElement tree@(Leaf current left right ) new 
        | current == new = Leaf new left right
        | current > new = Leaf current (replaceElement left new) right
        | current < new = Leaf current left (replaceElement right new)
        
getRightLeaf :: Ord(a) => BSTree a ->  BSTree a
getRightLeaf BSTree_Empty = BSTree_Empty
getRightLeaf (Leaf _ _ right) = right

getLeftLeaf :: Ord(a) => BSTree a ->  BSTree a
getLeftLeaf BSTree_Empty = BSTree_Empty
getLeftLeaf (Leaf _ left _) = left



--getHead (Node a esq dir) = a
{-
	===============================================
	=============== *FIM BSTree* ==================
	===============================================
-}
