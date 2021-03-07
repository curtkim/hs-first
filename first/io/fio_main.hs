-- type FilePath = String  (type synonym)
-- data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode 

import System.IO     
import Data.Char
    
withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
withFile' path mode f = do  
    handle <- openFile path mode   
    result <- f handle  
    hClose handle  
    return result 
    
main1 = do     
    withFile "LICENSE" ReadMode (\handle -> do  
        contents <- hGetContents handle     
        putStr contents) 

-- hGetContents = getContents
-- hGetLine, hPutStr, hPutStrLn, hGetChar


main2 = do  
    contents <- readFile "LICENSE"  
    putStr contents 

main3 = do     
    contents <- readFile "LICENSE"     
    writeFile "/tmp/LICENSE_CAP.txt" (map toUpper contents)

main4 = do     
    todoItem <- getLine  
    appendFile "todo.txt" (todoItem ++ "\n")  

main = main3