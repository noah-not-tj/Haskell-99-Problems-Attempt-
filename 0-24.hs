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
