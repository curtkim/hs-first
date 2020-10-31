import System.IO  
  
main = do  
    handle <- openFile "LICENSE" ReadMode  
    contents <- hGetContents handle  
    putStr contents  
    hClose handle  