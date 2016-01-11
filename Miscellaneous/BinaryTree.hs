-- Define the Tree data type.
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

-- Create a Tree with only one node.
singletonTree :: a -> Tree a
singletonTree x = Node x EmptyTree EmptyTree

-- Create a Tree from a List using foldr
treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

-- Insert a value into a Tree.
treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singletonTree x
treeInsert x (Node a l r)
		| x == a = Node x l r
		| x < a = Node a (treeInsert x l) r
		| otherwise = Node a l (treeInsert x r)

-- Check if a value exists in the Tree.
treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem _ EmptyTree = False
treeElem x (Node a l r)
		| x == a = True
		| x < a = treeElem x l
		| otherwise = treeElem x r

-- Check if a Tree t1 is a subTree of Tree t2
subTree :: (Ord a) => Tree a -> Tree a -> Bool
subTree EmptyTree _ = True
subTree _ EmptyTree = False
subTree (Node x1 l1 r1) (Node x2 l2 r2)
				| x1 == x2 = (subTree l1 l2) && (subTree r1 r2)
				| x1 < x2 = (subTree (Node x1 l1 r1) l2)
				| otherwise = (subTree (Node x1 l1 r1) r2)
