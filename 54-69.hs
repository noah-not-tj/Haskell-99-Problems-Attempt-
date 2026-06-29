--55  had to look at solution though
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)

createBalancedTree :: Int -> [Tree ()]
createBalancedTree 0 = [Empty]
createBalancedTree n
  | even n    = generate  (n `div` 2) ( (n `div` 2) - 1) 
                  ++ generate ( (n `div` 2) - 1) (n `div` 2)
                  
  | otherwise = generate (n `div` 2) (n `div` 2)
  
generate :: Int -> Int -> [Tree ()]
generate x y = [Branch () l r | l <- ltrees, r <- rtrees]
  where ltrees = createBalancedTree x
        rtrees = createBalancedTree y 

main :: IO ()
main = do
  print $ createBalancedTree 3
  
--56 completelybalancedtree and symmetric
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving Show

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
