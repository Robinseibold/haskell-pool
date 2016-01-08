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
