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

--27 Group the elements of a set into disjoint subsets LIKE WHAT THE HECK?!?!!?

--28 sort thing idk

--29 Fibonacci Numbers
fibonacci :: Integral a => a -> a
fibonacci 1 = 1
fibonacci 2 = 1
fibonacci x = fibonacci (x-1) + fibonacci (x-2)

main :: IO () 
main = do
  print $ map fibonacci [1..10]
--[1,1,2,3,5,8,13,21,34,55]

--Fibonacci numbers with matrix exponentiation


