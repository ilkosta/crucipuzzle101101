module Main (main) where

import Lib

-- from "Learn You a Haskell"
-- http://learnyouahaskell.com/input-and-output

import System.Environment   
import Data.List  

main = do  
    args <- getArgs                  -- IO [String]
    progName <- getProgName          -- IO String
    putStrLn "The arguments are:"  
    mapM_ putStrLn args
    putStrLn "The program name is:"  
    putStrLn progName


data DataSource = Filename String 
                  | UserInput

-- loadSchema UserInput =
--   getLine >>= 



insertSchema :: Schema -> String -> Schema
insertSchema schema line 
  | line == "." = schema
  | otherwise   = -- divide per virgola o caratteri strani la riga ed inserisce le lettere in schema
    splitWordsWhen ( == ',' || \x -> x `notElem` ['A'..'z']) : schema


splitWordsWhen     :: (Char -> Bool) -> String -> [String]
splitWordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : splitWordsWhen p s''
                            where (w, s'') = break p s'

readSchemaLine maxlen p chars line@(x:xs) 
  | p x                   = putStrLn "?" -- schema non conforme alle regole: carattere non ammesso
  | length chars > maxlen = putStrLn "?" -- schema non conforme alle regole: non rettangolare
  | otherwise = readSchemaLine maxlen p x:chars xs