import System.IO  
  
main = do  
    contents <- readFile "LICENSE"  
    putStr contents  