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
      [ [ 'c','i','a','o']
      , [ 'c','o','s','i']
      ]
    word = "io"
    tfl r c = 
      testCase ("impossibile dalla posizione (" ++ show r ++ "," ++ show c ++ ")" ) $
        readWord Nord word schema [(r,c)] @?= Nothing
  in
  testGroup "Lettura dello schema verso Nord"
  ( [ testCase "esito positivo da una posizione corretta" $
      readWord Nord word schema [(1,3)] @?= Just [(1,3),(0,3)]
    ] 
    ++ map (tfl 0) [0..3]
    ++ map (tfl 1) [0..2]
  )