-- import Prelude

-- lists

-- check if a list contains an element
elem' :: (Eq a) => a -> [a] -> Bool
elem' _ []       = False
elem' e (x : xs) = (e == x) || elem' x xs

-- remove duplicates from a list
nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x : xs)
  | x `elem` xs = nub xs
  | otherwise = x : nub xs

-- is list of ascending order
isAsc :: [Int] -> Bool
isAsc [] = True
isAsc [x] = True
isAsc (x : y : xs) =
  (x <= y) && isAsc (y : xs)

-- does a path between directed nodes exist
hasPath :: [(Int, Int)] -> Int -> Int -> Bool
-- -- hasPath [] _ _         = False
-- hasPath [(x,y)] a b    = x == a && y == b
-- hasPath ((x,y):xs) a b
--   | x == a    = hasPath xs y b
--   | otherwise = hasPath xs a b
hasPath [] x y = x == y
hasPath xs x y
  | x == y = True
  | otherwise =
      let xs' = [(a, b) | (a, b) <- xs, a /= x]
       in or [hasPath xs' a y | (a, b) <- xs, a == x]

hasPath' :: [(Int, Int)] -> Int -> Int -> Bool
hasPath' graph start finish = dfs [] [start]
  where
    dfs _ [] = False
    dfs marked (x : stack) =
      (x == finish) || dfs (x : marked) (neighbors marked x ++ stack)
    neighbors marked x = [fin | (st, fin) <- graph, st == x && notElem fin marked]

