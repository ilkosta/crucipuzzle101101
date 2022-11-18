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
    putStrLn "Enter the schema one line at a time, enter `.` to finish"
    status <- readSchema (LoadedSchema []) 
    case status of
      LoadedSchema schema -> do
        putStrLn "inserisci ora l'elenco di parole separate da spazi"
        status <- readWords (LoadedWords [])
        case status of
          LoadedWords wl ->
            case searchKey schema wl of
              Key k -> putStrLn k
              WordsAbsent _ -> notifyErr "WordsAbsent"
              NoKey -> notifyErr "NoKey"
          Res _ -> notifyErr "Res wl"
      Res _ -> notifyErr "Res schema"

notifyErr s = putStrLn $ "?" ++ s

data DataSource = Filename String 
                  | UserInput

readSchema :: Status -> IO Status
readSchema (LoadedSchema s) = do
  line <- getLine
  if line == "."
    then return (LoadedSchema s)
    else readSchema (loadSchema (LoadedSchema s) line)

readSchema s = return s

readWords :: Status -> IO Status
readWords (LoadedWords wl) = 
  loadWordList (LoadedWords wl) <$> getLine

readWord s = return s