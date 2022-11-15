module Lib
    ( someFunc
    , nth
    , at
    , Schema
    , Pos
    , Direction(..)
    , readWord
    , canMove
    , positions
    , startingPositions
    , wordsPosition
    , key
    ) where

import Data.List

{-| Schema

Lo schema e' una matrice di lettere maiuscole o minuscole
-}

type Schema = [String]
type Pos = (Int,Int)
type Key = [Pos]
type Match = [Pos] -- sequenza di celle che coperte dalle lettere di una parola
type Elenco = [String]

data Result =
  NonCompliantScheme
  | NonCompliantList 
  | WordsAbsent Elenco
  | NoKey 
  | Key String

result :: [String] -> String -> Result
result [] "" = NoKey
result [] k = Key k
result missingWords _ = WordsAbsent missingWords



data Direction = 
  Nord 
  | NordEst 
  | Est 
  | SudEst 
  | Sud
  | SudOvest
  | Ovest
  | NordOvest

directions :: [Direction]  
directions = [Nord , NordEst, Est, SudEst, Sud, SudOvest, Ovest, NordOvest]

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

readWord :: Direction -> String -> Schema -> Match -> Match
-- readWord dir w (r,c)  =
readWord _ "" _ m = m

-- quando arriva al singolo corattere della parola,
-- versione specializzata per l'ultima riga
readWord _ [s] schema ((r,c):xm) = 
  case schema `at` (r,c) of
    Nothing -> [] -- Just [(2,2)] -- Nothing -- 
    Just v -> 
      if s /= v then [] -- Just [(7,7)] -- Nothing -- 
      else (r,c):xm

readWord dir (s:xs) schema ((r,c):xm)  = 
  let 
    nm = nextPos dir (r,c):(r,c):xm
  in
    if canMove (r,c) dir (length xs) schema
    then
      case schema `at` (r,c) of
        Nothing -> [] -- Just [(2,2)] -- Nothing -- 
        Just v -> 
          if s /= v then [] -- Just ([(3,3), (100,100)] ++ ((r,c):xm)) -- Nothing --  
          else readWord dir xs schema nm
    else [] -- Just [(0,0)] -- Nothing


{-| enumera tutte le posizioni che compongono lo schema -}
positions schema = 
  let 
    generateCols r = 
      case nth r schema of
        Nothing -> [-1]   -- FIXME : error
        Just l  -> [0..length l-1]
  in      
    [ (r,c) 
      | r <- [ 0..length schema-1 ]
      , c <- generateCols r
    ]

{-| fornisce l'elenco di ciascuna posizione da cui possono partire le parole dell'elenco
    nella forma (posizione, parola)
-}
startingPositions :: Schema -> Elenco -> [(Pos,String)]
startingPositions schema words = 
  [ (pos,w)
  | pos <- positions schema
  , (pos,w) <- [ (pos,w) | w <- words, schema `at` pos == Just (head w) ]
  ]

{-| elenca tutte le posizioni nello schema 
    coperte da parole dell'elenco
    ogni posizione con la parola di riferimento
-}
matchedWordsPos :: Schema -> [(Pos,String)] -> (Match, String)
matchedWordsPos schema pw =
    [ (m,w) -- matched positions/word
    | (pos,w) <- pw  -- for each word and it's starting position
    , d <- directions       -- for each direction
    , m <- readWord d w schema [pos]  -- collect the matching positions if can read the word
    ]



{-| generate the results by searching each word in the schema
-}
searchKey :: Schema -> [String] -> Result
searchKey schema words =
  let 
    sp = startingPositions schema words
    wp' = wordsPosition schema sp
    wp = map fst wp'
    missingWords =  words \\ map snd wp'
    k = [ schema `at` p 
        | p <- positions schema 
        , p `notElem` wp
        ]
  in
    result missingWords k
        





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
nth n (_:xs)  
  | n > length xs = Nothing -- opt
  | otherwise     = nth (n-1) xs



{-| at evaluate to the value in the schema at the given position
-}
at :: Schema -> Pos -> Maybe Char
at [] _ = Nothing
at s (r,c) = 
  let
    row = nth r s
  in
    case row of 
      Nothing -> Nothing
      Just rw -> nth c rw
