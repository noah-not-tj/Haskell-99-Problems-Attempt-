--55 and 56 completelybalancedtree and symmetric
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving Show

leaf :: a -> Tree a
leaf x = Branch x (Empty) (Empty)

leafT :: Tree Char
leafT = leaf 'a'

createbalancedTree :: Int -> [Tree ()]
createbalancedTree 0 = [Empty ]
createbalancedTree x = [Branch () l r | 
              (ln, rn) <- distributions (x-1), 
              l <- createbalancedTree ln,
              r <- createbalancedTree rn]
  where
    distributions m
      | even m = [(m `div` 2, m `div` 2)]
      | otherwise = [(q, q + 1), (q + 1, q)]
        where q = m `div` 2
        
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 l2) (Branch _ r1 r2) = (mirror l1 l2) && (mirror r1 r2)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

main :: IO()
main = do
  let cp = symmetric (Branch 'x' (Branch 'x' Empty Empty) Empty)
  print cp

--57 Binary search Trees (this one was pretty fun)
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving Show
  
addTo :: Ord a => a -> Tree a -> Tree a
addTo x Empty = Branch x Empty Empty
addTo x (Branch n l r) 
  | x < n = (Branch n (addTo x l) r)
  | otherwise = (Branch n l (addTo x r))
  
construct :: Ord a => [a] -> Tree a
construct xs = foldl (\tree x -> addTo x tree) Empty xs

main :: IO()
main = do
  let cp = construct [4,1,2]
  print cp

-- 58 Symmetric and completely balanced binary trees
