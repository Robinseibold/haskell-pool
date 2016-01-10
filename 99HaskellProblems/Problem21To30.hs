import System.Random

-- Problem 21: Insert an element at a given position into a list. (List index starts with 1)
insertAt :: a -> [a] -> Int -> [a]
insertAt x xs n = take (n - 1) xs ++ [x] ++ drop (n - 1) xs

-- Problem 22: Create a list containing all integers within a given range.
range :: Int -> Int -> [Int]
range a b = [a, next a .. b]
		where next
			| b > a = succ
			| b < a = pred
			| otherwise = id

-- Problem 23: Extract a given number of randomly selected elements from a list.
randomSelect :: [a] -> Int -> IO [a]
randomSelect xs n 
		| n < 1 = return []
		| otherwise = do
			y <- randomRIO(0, (length xs) - 1)
			ys <- randomSelect xs (n - 1)
			return ((xs !! y):ys)


			



