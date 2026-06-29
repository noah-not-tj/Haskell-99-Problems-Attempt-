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

mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 l2) (Branch _ r1 r2) = (mirror l1 r2) && (mirror l2 r1)
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
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)
  
mirror :: Tree a -> Tree a -> Bool
mirror Empty Empty = True
mirror (Branch _ l1 l2) (Branch _ r1 r2) = (mirror l1 r2) && (mirror l2 r1)
mirror _ _ = False

symmetric :: Tree a -> Bool
symmetric Empty = True
symmetric (Branch _ l r) = mirror l r

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
        
symBalTrees :: Int -> [Tree ()]
symBalTrees n = filter symmetric (createBalancedTree n)

main :: IO ()
main = do
  print $ symBalTrees 5

-- 59 height balanced trees
data Tree a = Empty | Branch a (Tree a) (Tree a)
  deriving (Show)
  
heightBalancedTrees :: Int -> [Tree ()]
heightBalancedTrees n = filter checkHeight (generate n)
  
generate :: Int -> [Tree ()]
generate 0 = [Empty]
generate n = 
  [Branch () l r 
  | nL <- [0..(n-1)]
  , nR <- [0..(n-1)]
  , max nL nR == (n-1)
  , l <- generate nL
  , r <- generate nR
  ]
              
checkHeight :: Tree () -> Bool
checkHeight t = getHeight t /= -1

getHeight :: Tree () -> Int
getHeight Empty = 0
getHeight (Branch _ l r) 
  | lheight == -1 || rheight == -1  || abs (lheight - rheight ) > 1 = -1
  | otherwise = (max lheight rheight) + 1
    where lheight = getHeight l
          rheight = getHeight r

main :: IO ()
main = do
  print $ length $ heightBalancedTrees 4
