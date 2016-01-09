import Problem1To10

-- Data type for Problem 11, 12 and 13.
data Encoding a = Single a | Multiple Int a deriving Show

-- Help function for Problem 11 and 13.
createEncoding :: Int -> a -> Encoding a
createEncoding 1 x = Single x
createEncoding n x = Multiple n x


-- Problem 11: Modified run-length encoding.
encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified = map (\x -> createEncoding (length x) (head x)) . pack

-- Problem 12: Decode a run-length encoded list.
decode :: [Encoding a] -> [a]
decode = concat . map decodeOne
		where decodeOne (Single x) = [x]
		      decodeOne (Multiple n x) = replicate n x

-- Problem 13: Run-length encoding of a list (direct solution).
encodeModified' :: Eq a => [a] -> [Encoding a]
encodeModified' [] = []
encodeModified' [x] = [(Single x)]
encodeModified' xs = createEncoding (length $ takeWhile (==x) xs) x : encodeModified' (dropWhile (==x) xs)
			where x = head xs

-- Problem 14: Duplicate the elements of a list.
dubli :: [a] -> [a]
dubli [] = []
dubli (x:xs) = x : x : dubli xs

-- Problem 15: Replicate the elements of a list a given number of times.
repli :: [a] -> Int -> [a]
repli xs n = concat $ map (\x -> replicate n x) xs

-- Problem 16: Drop every N'th element from a list.
dropEvery :: [a] -> Int -> [a]
dropEvery [] _ = []
dropEvery xs n = fst ys ++ dropEvery (drop 1 $ snd ys) n
			where ys = splitAt (n - 1) xs 

-- Problem 17: Split a list into two parts; the length of the first part is given.
split :: [a] -> Int -> ([a], [a])
split xs n = (take n xs, drop n xs)

-- Problem 18: Extract a slice from a list.
slice :: [a] -> Int -> Int -> [a]
slice xs n1 n2 = take ((n2 - n1) + 1) $ drop (n1 - 1) xs

-- Problem 19: Rotate a list N places to the left.
rotate :: [a] -> Int -> [a]
rotate xs n = drop i xs ++ take i xs
		where i = if n > 0 then n else length xs + n

-- Problem 20: Remove the K'th element from a list.
removeAt :: Int -> [a] -> (a, [a])
removeAt n xs = (xs !! (n - 1), (fst ys) ++ (tail $ snd ys))
		where ys = split xs (n - 1)
