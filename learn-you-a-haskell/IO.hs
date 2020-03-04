import Control.Monad
import Data.Char 
import System.IO
import System.Environment   
import System.Directory  

-- main = do
--     putStrLn "Hello, what's your name?"
--     name <- getLine
--     putStrLn ("Hey " ++ name)

-- main = do
--     return "HAHA"
--     name <- getLine
--     return "HAHA 2"
--     putStrLn name

-- main = do
--     name <- return "Haha"
--     putStrLn (name ++ "!")

-- main = do
--     c <- getChar
--     when (c /= ' ') $ do
--         putChar c
--         main

-- main = do
--     a <- getLine
--     b <- getLine
--     c <- getLine
--     print [a,b,c]

-- -- ==

-- main = do
--     list <- sequence [getLine, getLine, getLine]
--     print list

-- main = forever $ do
--     putStr "Input:"
--     line <- getLine
--     putStrLn $ map toUpper line

-- palindrome = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
--     where isPalindrome xs = xs == reverse xs

-- main = interact palindrome

-- Read text from file

-- main = do
--     handle <- openFile "sample.txt" ReadMode
--     contents <- hGetContents handle
--     putStr contents
--     hClose handle

-- Todo cli

add :: [String] -> IO ()
add [path, text] = appendFile path (text ++ "/n")

view :: [String] -> IO ()  
view [fileName] = do  
    contents <- readFile fileName  
    let todoTasks = lines contents  
        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks  
    putStr $ unlines numberedTasks 

dispatch :: [(String, [String] -> IO ())]  
dispatch = [ ("add", add)
           , ("view", view)
           ] 

main = do
    (command:args) <- getArgs
    let (Just action) = lookup command dispatch
    action args
