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
  
  
