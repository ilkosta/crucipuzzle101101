module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib

main :: IO ()
main = defaultMain tests



tests :: TestTree
tests = 
  testGroup "tests"
  [ nthTest, schemaValueTest
  , readWordTest
  , canMoveTest
  ]

canMoveTest :: TestTree
canMoveTest = 
  let
    s :: Schema
    s = [ ['1','2','3','4']
        , ['5','6','7','8']
        , ['a','b','c','d']
        ]
  in
  testGroup "verifca canMove"
    [ testGroup "up"
      [ testCase "no se arrivato al limite" $
          canMove (0,0) Nord 0 s @?= False 
      , testCase "Up - ok 1 dist 1" $
          canMove (1,0) Nord 1 s @?= True
      , testCase "Up - ko 1 dist 2" $
          canMove (1,0) Nord 2 s @?= False
      , testCase "Up - ko 2 dist 4" $
          canMove (2,0) Nord 4 s @?= False
      , testCase "Up - ok 2 dist 2" $
          canMove (2,0) Nord 2 s @?= True
      , testCase "Up - ok 2 dist 3" $
          canMove (2,0) Nord 2 s @?= True
      ]
    , testGroup "down"
      [ testCase "no se arrivato al limite" $
          canMove (2,0) Sud 0 s @?= False
      , testCase "ok 1 dist 1" $
          canMove (1,0) Sud 1 s @?= True
      , testCase "ko 1 dist 2" $
          canMove (1,0) Sud 2 s @?= False
      , testCase "ko 2 dist 4" $
          canMove (0,0) Sud 4 s @?= False
      , testCase "ok 1 dist 1" $
          canMove (1,0) Sud 1 s @?= True
      , testCase "ok 0 dist 2" $
          canMove (0,0) Sud 2 s @?= True
      ]
    , testGroup "NordEst"
      [ testCase "no se arrivato al limite1" $
          canMove (0,0) NordEst 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (1,3) NordEst 0 s @?= False
      , testCase "no se arrivato al limite3" $
          canMove (0,3) NordEst 0 s @?= False

      , testCase "ok 1 dist 1" $
          canMove (1,2) NordEst 1 s @?= True

      , testCase "ko 1 dist 2" $
          canMove (1,2) NordEst 2 s @?= False
      , testCase "ko 2 dist 3" $
          canMove (2,1) NordEst 3 s @?= False

      , testCase "ok 2 dist 2" $
          canMove (2,1) NordEst 2 s @?= True          
      ]
    , testGroup "Est"
      [ testCase "no se arrivato al limite1" $
          canMove (0,3) Est 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (2,3) Est 0 s @?= False

      , testCase "ok 1 dist 1" $
          canMove (0,2) Est 1 s @?= True
      , testCase "ko 1 dist 2" $
          canMove (0,2) Est 2 s @?= False
      , testCase "ko 0 dist 4" $
          canMove (0,0) Est 4 s @?= False
      , testCase "ok 1 dist 2" $
          canMove (0,1) Est 2 s @?= True
      , testCase "ok 0 dist 3" $
          canMove (0,0) Est 3 s @?= True
      ]
    , testGroup "Ovest"
      [ testCase "no se arrivato al limite1" $
          canMove (0,0) Ovest 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (2,0) Ovest 0 s @?= False

      , testCase "ok 1 dist 1" $
          canMove (0,1) Ovest 1 s @?= True
      , testCase "ko 1 dist 2" $
          canMove (0,1) Ovest 2 s @?= False
      , testCase "ko 3 dist 4" $
          canMove (0,3) Ovest 4 s @?= False
      , testCase "ok 3 dist 3" $
          canMove (0,3) Ovest 3 s @?= True
      , testCase "ok 3 dist 2" $
          canMove (0,3) Ovest 2 s @?= True
      ]
    , testGroup "SudEst"
      [ testCase "no se arrivato al limite1" $
          canMove (2,0) SudEst 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (2,3) SudEst 0 s @?= False
      , testCase "no se arrivato al limite3" $
          canMove (0,3) SudEst 0 s @?= False

      , testCase "ok 1 dist 1." $
          canMove (1,2) SudEst 1 s @?= True      

      , testCase "ko 1 dist 2." $
          canMove (1,2) SudEst 2 s @?= False
      , testCase "ko 1 dist 2.." $
          canMove (1,1) SudEst 2 s @?= False
      , testCase "ko 0 dist 3" $
          canMove (0,0) SudEst 3 s @?= False

      , testCase "ok 0 dist 2" $
          canMove (0,1) SudEst 2 s @?= True          
      ]
    , testGroup "SudOvest"
      [ testCase "no se arrivato al limite1" $
          canMove (2,2) SudOvest 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (1,0) SudOvest 0 s @?= False
      , testCase "no se arrivato al limite3" $
          canMove (2,3) SudOvest 0 s @?= False

      , testCase "ok 1 dist 1." $
          canMove (1,1) SudOvest 1 s @?= True      

      , testCase "ko 1 dist 2." $
          canMove (1,1) SudOvest 2 s @?= False
      , testCase "ko 1 dist 2.." $
          canMove (1,2) SudOvest 2 s @?= False
      , testCase "ko 0 dist 3" $
          canMove (0,2) SudOvest 3 s @?= False

      , testCase "ok 0 dist 2" $
          canMove (0,2) SudOvest 2 s @?= True          
      ]
    , testGroup "NordOvest"
      [ testCase "no se arrivato al limite1" $
          canMove (0,2) NordOvest 0 s @?= False
      , testCase "no se arrivato al limite2" $
          canMove (1,0) NordOvest 0 s @?= False
      , testCase "no se arrivato al limite3" $
          canMove (0,0) NordOvest 0 s @?= False

      , testCase "ok 1 dist 1." $
          canMove (1,1) NordOvest 1 s @?= True      

      , testCase "ko 1 dist 2." $
          canMove (1,1) NordOvest 2 s @?= False
      , testCase "ko 1 dist 2.." $
          canMove (1,2) NordOvest 2 s @?= False
      , testCase "ko 2 dist 3" $
          canMove (2,2) NordOvest 3 s @?= False

      , testCase "ok 2 dist 2" $
          canMove (2,2) NordOvest 2 s @?= True          
      ]
    ]

nthTest :: TestTree
nthTest = 
  let 
    l = ['1','2','3','4']
  in
    testGroup "verifica nth"
    [ testCase "primo elemento se con indice 0" $
        nth 0 l @?= Just '1'
    , testCase "se non trovato torna Nothing" $
        nth 10 l @?= Nothing
    , testCase "estrae l'iesimo elmento" $
        nth 1 l @?= Just '2'
    ]

schemaValueTest :: TestTree
schemaValueTest =
  let
    s :: Schema
    s = [ ['1','2','3','4']
        , ['5','6','7','8']
        , ['a','b','c','d']
        ]
  in
    testGroup "verifca schemaValue"
    [ testCase "con schema vuoto torna Nothing" $
        schemaValue [] (0,0) @?= Nothing
    , testCase "con 0 torna la prima riga" $
        schemaValue s (0,0) @?= Just '1'
    , testCase "overflow di colonna sulla prima riga" $
        schemaValue s (0,5) @?= Nothing
    , testCase "lettura di 7" $
        schemaValue s (1,2) @?= Just '7'
    , testCase "overflow di riga" $
        schemaValue s (3,0) @?= Nothing
    , testCase "lettura di d" $
        schemaValue s (2,3) @?= Just 'd'
    ]


readWordTest :: TestTree
readWordTest = 
  let
    schema :: Schema
    schema = 
      [ [ 'c','o','s','i']
      , [ 'c','i','a','o']
      , [ 'c','o','s','i']
      , [ 'c','i','a','o']
      ]
    word = "io"
    wiso = "iso"

    tip r c = 
      testCase ("impossibile dalla posizione (" ++ show r ++ "," ++ show c ++ ")" ) $
        readWord Nord word schema [(r,c)] @?= Nothing
  in
    testGroup "Lettura dello schema"
    [
      testGroup "verso Nord"
      ( [ testCase "esito positivo da una posizione corretta" $
          readWord Nord word schema [(2,3)] @?= Just [(1,3),(2,3)]
        , testCase "esito positivo da una posizione corretta2" $
          readWord Nord word schema [(3,1)] @?= Just [(2,1),(3,1)]
        , testCase "esito positivo da una posizione corretta3" $
          readWord Nord word schema [(1,1)] @?= Just [(0,1),(1,1)]
        ] 
        ++ map (tip 0) [0..3]
        ++ map (tip 2) [0..2]
      )
    , testGroup "verso NordEst"
      [ testCase "esito positivo da una posizione corretta" $
          readWord Nord wiso schema [(3,1)] @?= Just [(1,3),(2,2),(3,1)]

      ]
    ]