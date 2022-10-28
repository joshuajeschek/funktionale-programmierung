-- Vorlesung
xor :: Bool -> Bool -> Bool
xor x y = (x || y) && not(x && y)

threeEqual :: Int -> Int -> Int -> Bool
threeEqual m n p = (m == n) && (n == p)

-- 01
-- a)
calcFibonacci :: Int -> Int
calcFibonacci i
  | i < 0     = error "bad number"
  | i == 0    = 0
  | i == 1    = 1
  | otherwise = calcFibonacci(i-1) + calcFibonacci(i-2)
-- b)
calcSum :: Int -> Int
calcSum n
  | n == 0    = 0
  | otherwise = n + calcSum(n-1)
-- c)
ggT :: Int -> Int -> Int
ggT a b
  | b == 0    = a
  | otherwise = ggT b (a `mod` b)

-- 02
-- a)
fourEqualA :: Int -> Int -> Int -> Int -> Bool
fourEqualA m n p o = threeEqual m n p && (p == o)
fourEqualB :: Int -> Int -> Int -> Int -> Bool
fourEqualB m n p o = (m == n) && (n == p) && (p == o)

-- b)
xor3A :: Bool -> Bool -> Bool -> Bool
xor3A x y z = x `xor` y `xor` z 
xor3B :: Bool -> Bool -> Bool -> Bool
xor3B x y z = (x || ((y || z) && not(y && z)))
  && not(x && ((y || z) && not(y && z)))
xor3C :: Bool -> Bool -> Bool -> Bool
xor3C x y z = xor (xor x y) z
xor3D :: Bool -> Bool -> Bool -> Bool
xor3D x y z = xor x (xor y z)

-- 03
-- a)
min2 :: Int -> Int -> Int
min2 a b
  | a < b     = a
  | otherwise = b
max2 :: Int -> Int -> Int
max2 a b
  | a > b     = a
  | otherwise = b
middleOfThree :: Int -> Int -> Int -> Int
middleOfThree a b c = a + b + c - min2 a (min2 b c) - max2 a (max2 b c)
-- b)
howManyEqualOfThree :: Int -> Int -> Int -> Int
howManyEqualOfThree a b c
  | (a == b) && (b == c)             = 3
  | (a == b) || (a == c) || (b == c) = 2
  | otherwise                        = 1
howManyEqualOfFour :: Int -> Int -> Int -> Int -> Int
howManyEqualOfFour a b c d
  | (a == b) && (b == c) && (c == d) = 4
  | otherwise                        = max2 (howManyEqualOfThree a b c) (howManyEqualOfThree b c d)

-- 04
nandA :: Bool -> Bool -> Bool
nandA x y
  | x && y    = False
  | otherwise = True
nandB :: Bool -> Bool -> Bool
nandB x y = not(x && y)
