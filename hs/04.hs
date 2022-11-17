-- 02
-- A)
flip :: (a -> b -> c) -> (b -> a -> c)
flip f a b = f b a

-- B)
total :: (Int -> Int) -> (Int -> Int)
total f n = sum [f i | i <- [0..n]]

-- C)
composeList :: [a -> a] -> (a -> a)
composeList [] = id
composeList (f:fs) = composeList fs . f

-- D)
integrate :: (Float -> Float) -> (Float -> Float -> Float)
integrate f a b = let h = (b - a) / 100_000 in sum [f((x+x+h)/2) * h | x <- [a, a+h..b]]

-- E)
calcOps :: [Float -> Float -> Float] -> Float -> Float -> [Float]
calcOps fs a b = [f a b | f <- fs]

-- F)
linearSearch :: (a -> Bool) -> [a] -> Int
linearSearch pred (x:xs)
  | pred x    = 0
  | otherwise = 1 + linearSearch pred xs

-- G)
quickSortGen :: (a -> a -> Bool) -> [a] -> [a]
quickSortGen _ [] = []
quickSortGen smallerThan (x:xs) = quickSortGen smallerThan [y | y <- xs, y `smallerThan` x] ++
                            [x] ++
                           quickSortGen smallerThan [y | y <- xs, not(y `smallerThan` x)]

quickSortStrings :: [String] -> [String]
quickSortStrings = quickSortGen (<=)
