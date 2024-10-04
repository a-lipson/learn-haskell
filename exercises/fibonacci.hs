import           Prelude hiding (zipWith3)


fibs = 0 : 1 : zipWith (+) fibs (tail fibs)

-- try making zipWithN
zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]
zipWith3 _ _ _ []               = []
zipWith3 _ _ [] _               = []
zipWith3 _ [] _ _               = []
zipWith3 f (x:xs) (y:ys) (z:zs) = f x y z : zipWith3 f xs ys zs

tribs = 0 : 0 : 1 : zipWith3 (\x y z -> x + y + z) tribs (tail tribs) (tail $ tail tribs)
