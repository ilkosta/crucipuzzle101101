module Lib
    ( someFunc
    , nth
    , schemaValue
    , Schema
    , Pos
    , Direction(..)
    , readWord
    ) where

someFunc :: IO ()
someFunc = putStrLn "ciao"


{-| Schema

Lo schema e' una matrice di lettere maiuscole o minuscole
-}

type Schema = [[Char]]
type Pos = (Int,Int)
type Key = [Pos]
type Match = [Pos] -- sequenza di celle che coperte dalle lettere di una parola
type Elenco = [String]

data Direction = 
  Nord 
  | NordEst 
  | Est 
  | SudEst 
  | Sud
  | SudOvest
  | Ovest
  | NordOvest

{-| 
f produce una lista di posizioni per tutte le parole
-}
-- e :: Schema -> f -> Key

-- getPos [s:xs] w r c =

readWord :: Direction -> String -> Schema -> Match -> Maybe Match
-- readWord dir w (r,c)  =
readWord _ "" _ m = Just m
readWord Nord (s:xs) schema ((r,c):xm)  = 
  if r <= length xs then Just [(1,1)] -- Nothing
  else
    case schemaValue schema (r,c) of
      Nothing -> Just [(2,2)] -- Nothing
      Just v -> 
        if s /= v then Just [(3,3)] -- Nothing
        else readWord Nord xs schema ((r-1,c):(r,c):xm )


-- readWord _ _ _ _ = Nothing -- FIXME: placeholder


{-| nth : return the corresponding elment in a list -}
nth :: Int -> [a] -> Maybe a
nth 0 (x:_) = Just x
nth _ [] = Nothing
nth n (_:xs) = 
  if n > length xs then Nothing -- opt
  else nth (n-1) xs

schemaValue :: Schema -> Pos -> Maybe Char
schemaValue [] _ = Nothing
schemaValue s (r,c) = 
  let
    row = nth r s
  in
    case row of 
      Nothing -> Nothing
      Just rw -> nth c rw
