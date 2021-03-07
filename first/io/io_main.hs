import Data.Char  
import Control.Monad (when, forever, forM)

main1 :: IO ()
main1 = putStrLn "hello, world"

main2 :: IO ()
main2 = do  
  putStrLn "Hello, what's your name?"  
  name <- getLine  
  putStrLn ("Hey " ++ name ++ ", you rock!") 

-- You may be wondering when to use <- and when to use let bindings?
main3 = do  
  putStrLn "What's your first name?"  
  firstName <- getLine  
  putStrLn "What's your last name?"  
  lastName <- getLine  
  let bigFirstName = map toUpper firstName  
      bigLastName = map toUpper lastName  
  putStrLn $ "hey " ++ bigFirstName ++ " " ++ bigLastName ++ ", how are you?"  


-- 
main4 = do   
  line <- getLine  
  if null line  
    then return () -- it makes an I/O action out of a pure value. it takes a value and wraps it up in a box
    else do -- else do()
      putStrLn $ reverseWords line  
      main4 -- recursive call
  
reverseWords :: String -> String  
reverseWords = unwords . map reverse . words 

-- 마지막 줄까지 실행된다. return이 종료의 의미가 아니다.
main5 = do  
  return ()  
  return "HAHAHA"  
  line <- getLine  
  return "BLAH BLAH BLAH"  
  return 4  
  putStrLn line 

-- return의 의미
-- return is sort of the opposite to <-
-- While return takes a value and wraps it up in a box, 
-- <- takes a box (and performs it) and takes the value out of it, binding it to a name
main6 = do  
    a <- return "hell"  
    b <- return "yeah!"  
    putStrLn $ a ++ " " ++ b  

-- putStr
main7 = do putStr "Hey, "  
           putStr "I'm "  
           putStrLn "Andy!" 

-- putChar
main8 = do putChar 't'  
           putChar 'e'  
           putChar 'h'

-- print
main9 = do print True  
           print 2  
           print "haha"  
           print 3.2  
           print [3,4,3]  

main10 = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            main10
        else return ()

main11 = do  
    c <- getChar  
    when (c /= ' ') $ do  
        putChar c  
        main11  

-- sequence :: [IO a] -> IO [a]
main12 = do  
    rs <- sequence [getLine, getLine, getLine]  
    print rs

main13 = sequence (map print [1,2,3,4,5]) 

main14 = mapM print [1,2,3]

main15 = forever $ do  
    putStr "Give me some input: "  
    l <- getLine  
    putStrLn $ map toUpper l 

-- forM :: (Monad m, Traversable t) => t a -> (a -> m b) -> m (t b)
-- forM : make an I/O action for every element in this list
-- 
-- multiline lambda
main16 = do   
    colors <- forM [1,2,3,4] (\a -> do  
        putStrLn $ "Which color do you associate with the number " ++ show a ++ "?"  
        color <- getLine  
        return color)  
    putStrLn "The colors that you associate with 1, 2, 3 and 4 are: "  
    mapM putStrLn colors -- forM colors putStrLn


-- getContents
main17 = do  
    contents <- getContents  
    putStr (shortLinesOnly contents)  
  
shortLinesOnly :: String -> String  
shortLinesOnly input =   
    let allLines = lines input  
        shortLines = filter (\line -> length line < 10) allLines  
        result = unlines shortLines  
    in  result  

-- interact (with lazy)
main18 = interact shortLinesOnly 

-- composition skill
main19 = interact $ unlines . filter ((<10) . length) . lines  


main = main19


