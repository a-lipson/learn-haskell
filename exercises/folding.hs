
-- folding

-- reverse a list
-- {-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
-- {-# HLINT ignore "Redundant where" #-}
rev :: [a] -> [a]
rev = foldl (flip (:)) []
-- rev = foldl (\x acc -> x : acc) []

-- give all prefixes of a list


prefixes :: [a] -> [[a]]
prefixes = foldr (\x acc -> [x] : map (x :) acc) []

-- prefixes = foldl f []
--
-- f :: [[a]] -> a -> [[a]]
-- f [] x = [[x]]
-- f xs x = xs ++ [last xs ++ [x]]
--
-- g :: [[a]] -> a -> [a]
-- g xs x = last xs ++ [x]

-- generate interpolation polynomial in lagrange form
lagrange :: [(Float, Float)] -> Float -> Float
lagrange ps x = foldr (\(x_i, y_i) -> (+) (y_i * l x_i)) 0 ps
  where
    l x_i = foldr (\(x_j, _) -> (*) (if x_i /= x_j then (x - x_j) / (x_i - x_j) else 1)) 1 ps

-- using list comprehensions instead of folding
lagrange_ :: [(Float, Float)] -> Float -> Float
lagrange_ points x = sum [y * l x_i | (x_i, y) <- points]
  where
    l x_i = product [(x - x_j) / (x_i - x_j) | (x_j, _) <- points, x_i /= x_j]


lagrange' :: [(Float, Float)] -> Float -> Float
lagrange' points x = foldr (\(x_i, y_i) acc -> acc + y_i * l x_i) 0 points
  where
    l x_i = foldr (\(x_j, _) acc -> if x_i /= x_j
                                    then acc * (x - x_j) / (x_i - x_j)
                                    else acc) 1 points

lagrange'' :: [(Float, Float)] -> Float -> Float
lagrange'' xs x = foldl (\acc (xj,y) -> acc + (y * l xj)) 0 xs
  where
    l xj = foldl (\acc (xk,_) -> if xj == xk
                                 then acc
                                 else acc * (x-xk) / (xj - xk) ) 1 xs

-- prefix tree
data Trie a = Leaf a | Node a [Trie a]

-- pre-order traversal fold for prefix tree
foldtrie :: (b -> a -> b) -> b -> Trie a -> b
foldtrie f acc (Leaf x)    = f acc x
foldtrie f acc (Node x xs) = foldl (foldtrie f) (f acc x) xs
