-- 01
-- a)
class Visible a where
  toString :: a -> String
  size :: a -> Int
instance Visible Char where
  toString ch = [ch]
  size _ = 1

instance (Visible a, Visible b) => Visible(a, b) where
  toString (a, b) = "(" ++ toString a ++ "," ++ toString b ++ ")"
  size _ = 2

-- b)
data Tree a = Leaf a | Branch (Tree a) (Tree a)
depth :: Tree a -> Int
depth (Leaf a)       = 0
depth (Branch t1 t2) = 1 + max (depth t1) (depth t2)
linearize :: Tree a -> [a]
linearize (Leaf n)       = [n]
linearize (Branch t1 t2) = linearize t1 ++ linearize t2

instance Visible a => Visible[a] where
  toString = concatMap toString
  size = foldr ((+) . size) 0

instance Visible a => Visible (Tree a) where
  toString tree = toString (linearize tree)
  size = depth

-- 03
-- a)
lshiftzip :: Int -> [Int] -> [Int]
lshiftzip 0 xs = xs
lshiftzip n xs = zipWith (+) xs (lshiftzip (n-1) (0 : init xs))
præsum :: [Int] -> [Int]
præsum xs = lshiftzip (length xs) xs

praefixSum:: [Int] -> [Int]
praefixSum [] = []
praefixSum x  = praefixSum (init x) ++ [sum x]
