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
