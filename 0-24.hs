8 compress 
compress :: Eq a => [a] -> [a]
compress []  = []
compress [x] = [x]
compress (x:y:xs)
  | x == y = compress (y:xs)
  | otherwise = x : compress (y:xs)
  
main :: IO() 
main = do
  let asdlfkj = compress "aaaabccaadeeee"
  print asdlfkj 

9 compress / pack elements into list of lists
firstgroup :: Eq a => [a] -> ([a], [a])
firstgroup (x : ys@(y : _)) 
  | x == y = let (d, r) = firstgroup ys in (x : d, r)
  | otherwise = ([x], ys)
firstgroup lonely = (lonely, [])

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = firstgroup xs
  
main :: IO() 
main = do
  let asdlfkj = pack "aaaabccaadeeee" 
  print asdlfkj 

10 encode string 
firstgroup :: Eq a => [a] -> ([a], [a])
firstgroup (x : ys@(y : _)) 
  | x == y = let (d, r) = firstgroup ys in (x : d, r)
  | otherwise = ([x], ys)
firstgroup lonely = (lonely, [])

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = firstgroup xs
 
len :: [a] -> (Int, a)
len xs@(x:_) = (length xs, x)
  
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map len $ pack xs
  
main :: IO() 
main = do
  let asdlfkj = encode "aaaabccaadeeee" 
  print asdlfkj 

10 modified run length encoding
data Encoding a = Single a | Multiple Int a
  deriving (Show)

firstgroup :: Eq a => [a] -> ([a], [a])
firstgroup (x : ys@(y : _)) 
  | x == y = let (d, r) = firstgroup ys in (x : d, r)
  | otherwise = ([x], ys)
firstgroup lonely = (lonely, [])

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = firstgroup xs
 
len :: [a] -> (Int, a)
len xs@(x:_) = (length xs, x)
  
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map len $ pack xs

encodeblah :: Eq a => (Int, a) -> Encoding a
encodeblah (num, x) 
  | num > 1 = Multiple num x
  | otherwise = Single x

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified [] = []
encodeModified xs = map encodeblah $ encode xs
  
main :: IO() 
main = do
  let asdlfkj = encodeModified "aaaabccaadeeee" 
  print asdlfkj 
  
11 decode encoded list 
data Encoding a = Single a | Multiple Int a
  deriving (Show)

firstgroup :: Eq a => [a] -> ([a], [a])
firstgroup (x : ys@(y : _)) 
  | x == y = let (d, r) = firstgroup ys in (x : d, r)
  | otherwise = ([x], ys)
firstgroup lonely = (lonely, [])

pack :: Eq a => [a] -> [[a]]
pack []  = []
pack xs = duplicates : pack remainder
  where (duplicates, remainder) = firstgroup xs
 
len :: [a] -> (Int, a)
len xs@(x:_) = (length xs, x)
  
encode :: Eq a => [a] -> [(Int, a)]
encode [] = []
encode xs = map len $ pack xs

encodeblah :: Eq a => (Int, a) -> Encoding a
encodeblah (num, x) 
  | num > 1 = Multiple num x
  | otherwise = Single x

encodeModified :: Eq a => [a] -> [Encoding a]
encodeModified [] = []
encodeModified xs = map encodeblah $ encode xs

decodiy :: Eq a => Encoding a -> [a]
decodiy (Multiple n x) = replicate n x 
decodiy (Single x) = [x] 

decodeModified :: Eq a => [Encoding a] -> [a]
decodeModified xs = foldl (++) [] blahblah
  where blahblah = map decodiy xs 
  
main :: IO() 
main = do
  let asdlfkj = decodeModified [Multiple 4 'a',Single 'b',Multiple 2 'c',Multiple 2 'a',Single 'd',Multiple 4 'e']
  print asdlfkj 

---------------13 didn't do

14 duplicate
dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x : x : dupli xs

main :: IO() 
main = do
  let asdlfkj = dupli [1, 1, 2] 
  print asdlfkj 

15 replicate
repli :: Int -> [a] -> [a] 
repli _ [] = []
repli 0 _ = []
repli num (x:xs) = (go num x ) ++ (repli num xs)
  where go :: Int -> a -> [a]
        go 0 _ = []
        go num x = x : (go (num - 1) x)

main :: IO() 
main = do
  let asdlfkj = repli 3 "abc" 
  print asdlfkj 

16 drop every N
dropEvery :: [a] -> Int -> [a]
dropEvery xs num = go xs num 1
  where go :: [a] -> Int -> Int -> [a]
        go [] _ _ = []
        go (x:xs) num count
          | count `mod` num == 0 = go xs num (count+1)
          | otherwise = x : (go xs num (count+1) )

main :: IO() 
main = do
  let asdlfkj = dropEvery "abcdefghik" 3
  print asdlfkj 

17 splitting a list (probably not expected approach)
split :: [a] -> Int -> ([a], [a])
split xs num = (take num xs, drop num xs)

main :: IO() 
main = do
  let asdlfkj = split "abcdefghik" 3
  print asdlfkj 

18 slicing a list one liner tuff 
slice :: [a] -> Int -> Int -> [a]
slice xs start end = map snd $ filter (\(n, _) -> (n >= start) && (n <= end)) $ zip [1..] xs

main :: IO() 
main = do
  let asdlfkj = slice "abcdefghijk" 3 7 
  print asdlfkj 
  
  
