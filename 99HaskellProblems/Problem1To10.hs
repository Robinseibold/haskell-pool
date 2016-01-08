module OneToTen  
( myLast	-- Problem 1 
, nextToLast	-- Problem 2
, elementAt	-- Problem 3
, myLength	-- Problem 4
, myReverse	-- Problem 5
, isPalindrome	-- Problem 6
, flatten	-- Problem 7
, compress	-- Problem 8
, pack		-- Problem 9
, encode	-- Problem 10
) where 

import Data.List

-- Problem 1: Find the last element of a list.
myLast :: [a] -> a
myLast [] = error "Can't get last element of an empty list"
myLast [x] = x
myLast (_ : xs) = myLast xs

-- Problem 2: Find the last but one element of a list.
nextToLast :: [a] -> a
nextToLast [] = error "Can't get the second last element of an empty list"
nextToLast [x] = error "Can't get the second last element of an list with one element"
nextToLast [x, _]  = x
nextToLast (_ : xs) = nextToLast xs

-- Problem 3: Find the K'th element of a list. The first element in the list is number 1.
elementAt :: Int -> [b] -> b
elementAt _ [] = error "Can't get element at specific index from an empty list"
elementAt 1 (x : _) = x
elementAt i (_ : xs)
		| i < smallestIndex = error "Invalid index"
		| i > biggestIndex = error "Invalid index"
		| otherwise = elementAt (i - 1) xs
		where (smallestIndex, biggestIndex) = (1, (length xs) + 1)

-- Problem 4: Find the number of elements of a list.
myLength :: [a] -> Int
myLength [] = 0
myLength (_ : xs) = 1 + myLength xs
		
-- Problem 5: Reverse a list.
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x : xs) = myReverse xs ++ [x]

-- Problem 6: Find out whether a list is a palindrome.
isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome xs = myReverse xs == xs

-- Problem 7: Flatten a nested list structure.
data NestedList a = Elem a | List [NestedList a]
flatten :: NestedList a -> [a]
flatten (Elem x) = [x]
flatten (List (x : xs)) = flatten x ++ flatten (List xs)
flatten (List []) = []  

-- Problem 8: Eliminate consecutive duplicates of list elements.
compress :: Eq a => [a] -> [a]
compress [] = []
compress [x] = [x]
compress (x1 : x2 : xs) = if x1 == x2 then compress (x2 : xs) else x1 : compress (x2 : xs)

compress' :: Eq a => [a] -> [a]
compress' = map head . group

-- Problem 9: Pack consecutive duplicates of list elements into sublists.                   
pack :: Eq a => [a] -> [[a]]
pack [] = []
pack xs = [takeWhile (== head xs) xs] ++ pack (dropWhile (== head xs) xs)

pack' :: Eq a => [a] -> [[a]]
pack' = group

-- Problem 10: Run-length encoding of a list.
encode :: Eq a => [a] -> [(Int, a)]
encode = map (\x -> (length x, head x)) . pack
