--46 Truth tables for logical expressions
import Text.Printf (printf)

type BoolFunc = Bool -> Bool -> Bool

and' :: BoolFunc
and' a b 
  | a && b    = True
  | otherwise = False

nand' :: BoolFunc
nand' a b = not $ and' a b
  
or' :: BoolFunc
or' a b 
  | a         = True
  | b         = True
  | otherwise = False

nor' :: BoolFunc
nor' a b = not $ or' a b

xor' :: BoolFunc
xor' a b 
  | a && b    = False
  | otherwise = or' a b

table :: BoolFunc -> [(Bool, Bool, Bool)]
table f = [(False, False, f False False),
              (False, True, f False True),
              (True, False, f True False),
              (True, True, f True True)]
              
printRow :: (Bool, Bool, Bool) -> IO ()
printRow (a,b,o) = do
  printf "| %-6s | %-6s | %-6s |\n" (show a) (show b) (show o)
  printf "| %-6d | %-6d | %-6d |\n" (fromEnum a) (fromEnum b) (fromEnum o)

  putStrLn "|--------------------------|"
                    
printTable :: [(Bool, Bool, Bool)] -> IO ()
printTable xs = do
  mapM_ printRow xs 

  
main :: IO () 
main = do 
    putStrLn "| A      |B       |O       |"
    putStrLn "|--------------------------|"
    let cp = table (\a b -> (and' a b))
    printTable cp 
    --putStrLn "|--------------------------|"

--47 Universal Logic Gates (ABSOLUTE INSANITY)
nand' :: Bool -> Bool -> Bool
nand' a b = not (a && b)

evaluateCircuit :: [(Int, Int)] -> Bool -> Bool -> Bool
evaluateCircuit gates a b = last $ evaluateNodes gates []
  where 
    evaluateNodes :: [(Int, Int)] -> [Bool] -> [Bool]
    evaluateNodes [] list = list
    evaluateNodes ((x,y):xs) list =
      let valX = check x
          valY = check y
          curr_result = valX `nand'` valY
      in evaluateNodes xs $ list ++ [curr_result]
      where 
        check :: Int -> Bool
        check (-1) = a
        check (-2) = b
        check x    = list !! (x-1)
        
main :: IO ()
main = do
  let cp = evaluateCircuit [(-1,-1),(-2,-2),(1,2)] False False
  print cp
