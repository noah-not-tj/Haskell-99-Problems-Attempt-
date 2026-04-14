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

--30 Fibonacci numbers with matrix exponentiation
type Matrix a = (a,a,a,a)

multM :: Num a => Matrix a -> Matrix a -> Matrix a
multM (a,b,c,d) (e,f,g,h) =
  ( a*e + b*g
  , a*f + b*h
  , c*e + d*g
  , c*f + d*h
  )

powM :: (Integral n, Num a) => n -> Matrix a -> Matrix a
powM 0 _ = (1,0,0,1)
powM 1 m = m
powM n m
  | even n = powM (n `div` 2) (multM m m)
  | otherwise = multM m (powM (n-1) m)

fibonacci' :: Integral a => a -> a
fibonacci' n
  | n <= 1 = 1
  | n == 2 = 1
  | otherwise =
      let (_, fn, _, _) = powM (n) (1,1,1,0)
      in fn

main :: IO ()
main = print $ map fibonacci' [1..10]

-- 31 isprime
isqrt :: Integral a => a-> a
isqrt = floor . sqrt . fromIntegral
     
isPrime :: Integral a => a -> Bool
isPrime k
  |k>1 = null [x | x <- [2..isqrt k], k `mod` x == 0]
  |otherwise = False

main :: IO ()
main = do
  let yay = isPrime 15
  print yay

--32 gcd 
myGCD :: Integral a => a -> a -> a
myGCD 0 x = x
myGCD x 0 = x
myGCD a b = myGCD b $ a `rem` b

main :: IO ()
main = do
  let gc = myGCD 221 559
  print gc 

-- 33 coprime
prime :: Integral a => a -> a -> Bool
coprime x y = myGCD x y == 1

myGCD :: Integral a => a -> a -> a
myGCD 0 x = x
myGCD x 0 = x
myGCD a b = myGCD b ( a `rem` b )

main :: IO ()
main = do
  let cp = coprime 1173 1547
  print cp

-- 34 totient, Probably not optimal but oh well
import Data.List (nub)

totient :: Integral a => a -> a 
totient 1 = 1
totient n = 
    let p = factors 2 n 
        g = nub p 
    in n * product [p - 1 | p <- g] `div` product g

factors :: Integral a => a -> a -> [a]
factors _ 1 = []
factors base n 
    | base * base > n = [n]
    | n `mod` base == 0 = base : factors base (n `div` base)
    | otherwise = factors (base + 1) n

main :: IO ()
main = do 
    let cp = totient 20
    print cp 

-- 35 List of prime factors
--(Just reused the same thing from the last problem)
factors :: Integral a => a -> a -> [a]
factors _ 1 = []
factors base n 
    | base * base > n = [n]
    | n `mod` base == 0 = base : factors base (n `div` base)
    | otherwise = factors (base + 1) n

main :: IO ()
main = do 
    let cp = factors 2 315
    print cp 

--36 runtime encode the primes
primeFactorsMultiplicity :: Integral a => a -> [(a, a)]
primeFactorsMultiplicity n = 
  let list = primeFactors n
  in encode list
    
primeFactors :: Integral a => a -> [a]
primeFactors n = factors 2 n

factors :: Integral a => a -> a -> [a]
factors _ 1 = []
factors base n 
    | base * base > n = [n]
    | n `mod` base == 0 = base : factors base (n `div` base)
    | otherwise = factors (base + 1) n

encode :: Integral a => [a] -> [(a, a)]
encode = map (\xs -> (head xs, fromIntegral (length xs))) . group

main :: IO ()
main = do 
    let cp = primeFactorsMultiplicity 315
    print cp 

-- 37 apparently I did 34 the right but unexpected way...

--38 

--39 list of primes from x to yed
isqrt :: Integral a => a-> a
isqrt = floor . sqrt . fromIntegral
     
isPrime :: Integral a => a -> Bool
isPrime k
  |k>1 = null [x | x <- [2..isqrt k], k `mod` x == 0]
  |otherwise = False
  
primesR :: Integral a => a -> a -> [a]
primesR x y = 
  [x | x <- [x..y], isPrime x]

main :: IO ()
main = do 
    let cp = primesR 10 20
    print cp 

-- 40 !!!! Goldbach's Conjecture oh heck yeah
isqrt :: Integral a => a-> a
isqrt = floor . sqrt . fromIntegral
     
isPrime :: Integral a => a -> Bool
isPrime k
  |k>1 = null [x | x <- [2..isqrt k], k `mod` x == 0]
  |otherwise = False
  
primesR :: Integral a => a -> a -> [a]
primesR x y = 
  [x | x <- [x..y], isPrime x]

goldbach :: Integral a => a -> (a, a)
goldbach n = 
  let list = primesR 1 n
  in head [(x, y) | x <- primesR 1 n, 
                    let y = n-x, isPrime y]

main :: IO ()
main = do 
    let cp = goldbach 12
    print cp 

