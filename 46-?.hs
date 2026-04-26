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

--47 
