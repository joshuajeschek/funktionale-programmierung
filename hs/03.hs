-- 01
-- A
orderTriple :: (Int, Int, Int) -> (Int, Int, Int)
orderTriple (x, y, z)
  | x <= y && y <= z = (x, y, z)
  | x > y            = orderTriple (y, x, z)
  | otherwise        = orderTriple (x, z, y)

-- B
rotateQuadruple :: (Int, Int, Int, Int) -> Int -> (Int, Int, Int, Int)
rotateQuadruple (w, x, y, z) a
  | a > 0     = rotateQuadruple (z, w, x, y) (a-1)
  | a < 0     = rotateQuadruple (x, y, z, w) (a+1)
  | otherwise = (w, x, y, z)

-- C
maxPairs :: [(Int, Int)] -> (Int, Int)
maxPairs []  = (0, 0)
maxPairs [x] = x
maxPairs (x:xs)
  | uncurry (+) x >= uncurry (+) (maxPairs xs) = x
  | otherwise                                  = maxPairs xs

-- 02
-- A
scalar :: [Int] -> [Int] -> Int
scalar [] [] = 0
scalar (x:xs) (y:ys) = x * y + scalar xs ys
scalar xs ys = error "Vektoren ungleich lang"

-- B
isPalin :: [Int] -> Bool
isPalin xs = reverse xs == xs

-- C
matchesA :: [Int] -> Int -> [Int]
matchesA xs y = [x | x <- xs, x == y]

matchesB :: [Int] -> Int -> [Int]
matchesB [] y = []
matchesB (x:xs) y
  | x == y    = x : matchesB xs y
  | otherwise = matchesB xs y

-- 03
-- A
merge :: [Int] -> [Int] -> [Int]
merge (x:xs) (y:ys)
  | x <= y = x : merge xs (y:ys)
  | otherwise = y : merge (x:xs) ys
merge xs ys = xs ++ ys -- at least one list is empty

mergeSort :: [Int] -> [Int]
mergeSort [x] = [x]
mergeSort xs = merge(mergeSort(take (length xs `div` 2) xs))(mergeSort(drop (length xs `div` 2) xs))

-- B
nor3A :: Bool -> Bool -> Bool -> Bool
nor3A False False False = True
nor3A _ _ _ = False

nor3B :: Bool -> Bool -> Bool -> Bool
nor3B a b c
  | a         = False
  | b         = False
  | c         = False
  | otherwise = True
