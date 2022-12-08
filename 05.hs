-- 01
-- A
fac :: Int -> Int
fac 0 = 0
fac x = foldl (*) x [1..x-1]

-- B
sumQu :: Int -> Int
-- sumQu n = foldr (+) (n^2) (map (^2) [0..(n-1)])
sumQu n = foldr ((+) . (^2)) (n^2) [0..(n-1)]

-- C
length :: [a] -> Int
-- length as = foldl (+) 0 [1 | a <- as]
length as = foldr (+) 0 [1 | a <- as]

-- D
reverse :: [a] -> [a]
-- reverse = foldr (\x xs -> xs ++ [x]) []
reverse = foldl (\xs x -> x:xs) []


-- 02
-- A
data FloatExpr = Lit Float |
                 Add FloatExpr FloatExpr |
                 Mult FloatExpr FloatExpr |
                 Root FloatExpr |
                 Abs FloatExpr
evaluate :: FloatExpr -> Float
evaluate (Lit n) = n
evaluate (Add e1 e2) = evaluate e1 + evaluate e2
evaluate (Mult e1 e2) = evaluate e1 * evaluate e2
evaluate (Root n) = sqrt $ evaluate n
evaluate (Abs n) = abs $ evaluate n

