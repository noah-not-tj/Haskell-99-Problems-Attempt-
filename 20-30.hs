--25 random permutation of list

import System.Random

randomPermute :: RandomGen g => [a] -> g -> ([a], g)
randomPermute [] g = ([], g)
randomPermute xs g =
  let (index, g') = randomR (0, length xs - 1) g
      (left, (selected:right)) = splitAt index xs
      (rest, g'') = randomPermute (left ++ right) g'
  in (selected:rest, g'')

main :: IO () 
main = do
  print (fst $ randomPermute [1..10] $ mkStdGen 111)


--26 k combinations of a list 
combinations :: Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations n (x:xs) = 
  (map (x:) $ combinations (n-1) xs )++ combinations n xs


main :: IO () 
main = do
  print (length $ combinations 3 [1..12])
