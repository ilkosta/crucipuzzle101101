module Main (main) where

import Lib

import System.Environment   
import System.Exit
import Data.List  

{-| Crucipuzzle: 
    givena a rectangular schema of  letters and a list of words
    exclude all the letters from the schema that corresponds to words
    end extract the key contained inside the schema


    the program can start with 2 arguments indicating 
    - the file containing the schema
    - the file containing the words to search inside the schema  

    or will ask the schema and the word list to the user interactively
-}
main :: IO ()
main = do
  args <- getArgs
  case args of
    -- schema and word list from files
    [ fs, fwl ] -> do
      status <- foldr loadSchema (LoadedSchema []) . lines <$> readFile fs
      case status of
        Res _ -> notifyErr --"Res schema"
        LoadedSchema schema -> do          
          status <- loadWordList (LoadedWords []) <$> readFile fwl
          case status of
            Res _ -> notifyErr --"Res wl"
            LoadedWords wl -> search schema wl
    
    -- interactivity version
    _ -> do
      name <- getProgName
      putStrLn $ "Ciao, il programma puo' essere invocato anche come: " ++ name ++ " file_schema file_words\n"
      putStrLn "Enter the schema one line at a time, enter `.` to finish:"
      status <- interactiveReadSchema (LoadedSchema []) 
      case status of
        Res _ -> notifyErr --"Res schema"
        LoadedSchema schema -> do
          status <- interactiveReadWordList
          case status of
            Res _ -> notifyErr --"Res wl"
            LoadedWords wl -> search schema wl
            
        

notifyErr {-s-} = die "?" -- ++ s

search schema wl = 
  case searchKey schema wl of
    Key k -> putStrLn k 
    WordsAbsent _ -> notifyErr --"WordsAbsent"
    NoKey -> notifyErr --"NoKey"

interactiveReadWordList = 
  putStrLn "inserisci ora l'elenco di parole separate da spazi" >> 
  readWords (LoadedWords [])


interactiveReadSchema :: Status -> IO Status
interactiveReadSchema (LoadedSchema s) = do
  line <- getLine
  if line == "."
    then return (LoadedSchema s)
    else interactiveReadSchema (loadSchema line (LoadedSchema s) )

interactiveReadSchema s = return s

readWords :: Status -> IO Status
readWords (LoadedWords wl) = 
  loadWordList (LoadedWords wl) <$> getLine

readWord s = return s
