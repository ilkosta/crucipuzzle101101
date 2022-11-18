module Lib
    ( at
    , Schema
    , Pos
    , Direction(..)
    , readWord
    , canMove
    , positions
    , startingPositions
    , matchedWordsPos
    , searchKey
    , loadSchema
    , loadWordList
    , Status(..)
    , Result(..)
    ) where

import Data.List((\\))
import Data.Maybe

{-| Schema

Lo schema e' una matrice di lettere maiuscole o minuscole
-}

type Schema = [String]
type Pos = (Int,Int)
type Key = [Pos]
type Match = [Pos] -- sequenza di celle che coperte dalle lettere di una parola
type Elenco = [String]

data Status = 
  LoadedSchema Schema
  | LoadedWords [String]
  | Res Result


data Result =
  NonCompliantScheme Schema
  | NonCompliantList [String]
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

-- per il compilatore
readWord _ _ _ _ = []


{-| enumera tutte le posizioni che compongono lo schema -}
positions :: Foldable t => [t a] -> [(Int, Int)]
positions schema = 
  [ (r,c) 
    | r <- [ 0..length schema -1 ]
    , c <- [ 0..length (schema !! r) -1]
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
matchedWordsPos :: Schema -> [(Pos,String)] -> [(Pos, String)]
matchedWordsPos schema pw =
    [ (m,w) -- matched positions/word
    | (pos,w) <- pw  -- for each word and it's starting position
    , d <- directions       -- for each direction
    , m <- readWord d w schema [pos]  -- collect the matching positions if can read the word
    ]



{-| generate the results by searching each word in the schema
-}
searchKey :: Schema -> [String] -> Result
searchKey schema wl =
  let 
    sp = startingPositions schema wl
    wp' = matchedWordsPos schema sp
    wp = map fst wp'
    missingWords =  wl \\ map snd wp'
    k = map fromJust 
        ( filter isJust 
            [ schema `at` p 
            | p <- positions schema 
            , p `notElem` wp        
            ]
        )
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



-- {-| nth : return the corresponding elment in a list -}
-- nth :: Int -> [a] -> Maybe a
-- nth 0 (x:_) = Just x
-- nth _ [] = Nothing
-- nth n (_:xs)  
--   | n > length xs = Nothing -- opt
--   | otherwise     = nth (n-1) xs



{-| at evaluate to the value in the schema at the given position
-}
at :: Schema -> Pos -> Maybe Char
at [] _ = Nothing
at s (r,c) 
  | r >= length s         = Nothing
  | c >= length (s !! r)  = Nothing
  | otherwise             = Just ((s !! r) !! c)


  ----
charSeq :: [Char]
charSeq = ['A'..'z']

acceptableChar :: Char -> Bool
acceptableChar c = c `elem` charSeq

string2charSeq :: [Char] -> [Char]
string2charSeq s = [c | c <- s, acceptableChar c]

notAcceptableStr :: [Char] -> Bool
notAcceptableStr s = not (null (s \\ charSeq))

{-| permette l'inserimento itertivo di una riga di caratteri come riga dello schema
-}
loadSchema :: Status -> String {- line -} -> Status
loadSchema (Res r) _ = Res r
loadSchema (LoadedSchema []) line
  | notAcceptableStr line
    = Res . NonCompliantScheme $ []
  | otherwise 
    = LoadedSchema [string2charSeq line]
loadSchema (LoadedSchema s) line
  | notAcceptableStr line
    = Res . NonCompliantScheme $ s
  | length (head s) == length line
    = LoadedSchema $ line : s
  | otherwise = Res . NonCompliantScheme $ s
loadSchema s _ = s

{-| permette l'inserimento iterativo di una riga contenente parole nella lista di parole
-}
loadWordList :: Status -> String {- line -} -> Status
loadWordList (Res r) _ = Res r
loadWordList (LoadedWords wl) line = 
  let 
    -- parole accettabili
    wl' = [ w | w <- words line, null (w \\ charSeq) ]
    allAcceptableWords = length (words line) == length wl'
  in
    if allAcceptableWords 
      then LoadedWords $ wl ++ wl'
      else Res . NonCompliantList $ wl
loadWordList s _ = s      