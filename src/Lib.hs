module Lib
    ( someFunc
    , nth
    , schemaValue
    , Schema
    , Pos
    , Direction(..)
    , readWord
    , canMove
    ) where

someFunc :: IO ()
someFunc = putStrLn "ciao"


{-| Schema

Lo schema e' una matrice di lettere maiuscole o minuscole
-}

type Schema = [String]
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


type Distance = Int

canMoveUp     :: Pos -> Distance -> Bool
canMoveUp (0,_) _ = False
canMoveUp (r,_) d = r >= d


canMoveLeft   :: Pos -> Distance -> Bool
canMoveLeft  (_,0) _ = False
canMoveLeft  (_,c) d = c >= d


canMoveDown   :: Pos -> Distance -> Schema -> Bool
canMoveDown (r,_) d s 
  | d == 0 = False
  | length s > r = length s - r > d
  | otherwise = False


canMoveRight  :: Pos -> Distance -> Schema -> Bool
canMoveRight (_,c) d s 
  | d == 0 = False
  | (length . head) s > c = (length . head) s - c > d
  | otherwise = False



canMove :: Pos -> Direction -> Distance -> Schema -> Bool
canMove pos Nord dist _ = 
  canMoveUp pos dist

canMove pos NordEst dist schema = 
  canMoveUp pos dist && canMoveRight pos dist schema

canMove pos NordOvest dist _ =
  canMoveUp pos dist && canMoveLeft pos dist

canMove pos Sud dist schema = 
  canMoveDown pos dist schema

canMove pos SudEst dist schema = 
  canMoveDown pos dist schema && canMoveRight pos dist schema

canMove pos SudOvest dist schema =
  canMoveDown pos dist schema && canMoveLeft pos dist

canMove pos Est dist schema =
  canMoveRight pos dist schema

canMove pos Ovest dist _ =
  canMoveLeft pos dist  





{-| 
f produce una lista di posizioni per tutte le parole
-}
-- e :: Schema -> f -> Key

-- getPos [s:xs] w r c =

readWord :: Direction -> String -> Schema -> Match -> Maybe Match
-- readWord dir w (r,c)  =
readWord _ "" _ m = Just m

-- quando arriva al singolo corattere della parola,
-- versione specializzata per l'ultima riga
readWord _ [s] schema ((r,c):xm) = 
  case schemaValue schema (r,c) of
    Nothing -> Nothing -- Just [(2,2)]
    Just v -> 
      if s /= v then Nothing -- Just [(7,7)]
      else Just ((r,c):xm)

readWord dir (s:xs) schema ((r,c):xm)  = 
  let 
    nm = nextPos dir (r,c):(r,c):xm
  in
    if canMove (r,c) dir (1 + length xs) schema
    then
      case schemaValue schema (r,c) of
        Nothing -> Nothing -- Just [(2,2)]
        Just v -> 
          if s /= v then Nothing -- Just [(3,3)] 
          else readWord dir xs schema nm
    else Just [(0,0)] -- Nothing

    


-- readWord _ _ _ _ = Nothing -- FIXME: placeholder


nextPos :: Direction -> Pos -> Pos
nextPos Nord      (r,c) = (r-1,c)
nextPos NordEst   (r,c) = (r-1,c+1)
nextPos Est       (r,c) = (r,c+1)
nextPos SudEst    (r,c) = (r+1,c+1)
nextPos Sud       (r,c) = (r+1,c)
nextPos SudOvest  (r,c) = (r+1,c-1)
nextPos Ovest     (r,c) = (r,c-1)
nextPos NordOvest (r,c) = (r-1,c-1)



{-| nth : return the corresponding elment in a list -}
nth :: Int -> [a] -> Maybe a
nth 0 (x:_) = Just x
nth _ [] = Nothing
nth n (_:xs) = 
  if n > length xs then Nothing -- opt
  else nth (n-1) xs



{-| schemaValue evaluate to the value in the schema at the given position
-}
schemaValue :: Schema -> Pos -> Maybe Char
schemaValue [] _ = Nothing
schemaValue s (r,c) = 
  let
    row = nth r s
  in
    case row of 
      Nothing -> Nothing
      Just rw -> nth c rw
