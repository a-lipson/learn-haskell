import           Data.List       hiding (insert)
import           Test.QuickCheck

data Tree a = Leaf | Node (Tree a) a (Tree a)
  deriving Show

-- take for trees, cut at a certain height
cut :: Integer -> Tree a -> Tree a
cut 0 _            = Leaf
cut n Leaf         = Leaf
cut n (Node l v r) = Node (cut (n-1) l) v (cut (n-1) r)

-- infinite tuple tree tracking path direction count
infTupTree :: Tree (Integer, Integer)
infTupTree = aux (0,0)
  where
    aux (l,r) = Node (aux (l+1,r)) (l,r) (aux (l,r+1))



-- insert an ordered value into a tree
insert :: (Ord a) => a -> Tree a -> Tree a
insert v Leaf = Node Leaf v Leaf
insert v (Node l t r)
  | v <= t = Node (insert v l) t r
  | v > t  = Node l t (insert v r)

-- returns in order traversal of a tree
inorder :: Tree a -> [a]
inorder Leaf         = []
inorder (Node l v r) = inorder l ++ [v] ++ inorder r


-- quick check property if in order traversal of inserted tree is sorted
prop_IIS xs = sort xs === xs'
  where
    types = xs :: [Int]
    xs' = inorder $ foldr insert Leaf xs
