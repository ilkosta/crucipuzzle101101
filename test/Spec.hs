module Main where

import Test.Tasty
import Test.Tasty.HUnit
import Lib

main :: IO ()
main = defaultMain tests



tests :: TestTree
tests = 
  testGroup "tests"
  [ schemaValueTest
  , canMoveTest
  , readWordTest
  , positionsTest
  , startingPositionsTests
  , matchedWordsPosTests
  , searchKeyTests
  , loadSchemaTest
  , loadWordListTest
  ]

loadSchemaTest :: TestTree
loadSchemaTest =
  let
    terr (Res (NonCompliantScheme s)) msg = assertBool "ok" True
    terr (LoadedSchema s) msg = 
      assertFailure $ msg ++ " : LoadedSchema " ++ show s
    terr (Res _ ) msg = 
      assertFailure $ msg ++ " : generic Res"

    tok (Res (NonCompliantScheme s)) msg = 
      assertFailure $ msg ++ " : NonCompliantScheme " ++ show s
    tok _ _ = assertBool "---" True
  in
  testGroup "verifica dello schema" 
  [ testCase "schema non rettangolo" $
    terr (loadSchema "abcd" $ LoadedSchema ["abc"]) 
      "tentativo di inserire abcd -> [abc]"
  , testCase "schema con caratteri non validi" $
    terr (loadSchema "ab.d" $ LoadedSchema ["abc"])  
      "tentativo di inserire ab.d -> [abc]"
  , testCase "schema con caratteri non validi" $
    terr (loadSchema "ab1d" $ LoadedSchema ["abc"])  
      "tentativo di inserire ab1d -> [abc]"
  , testCase "schema corretto" $
    tok (loadSchema "abc" $ LoadedSchema ["abd"]) 
      "tentativo di inserire abd -> [abc]"
  ]
  
loadWordListTest :: TestTree
loadWordListTest =
  let
    terr (Res (NonCompliantList wl)) msg = assertBool "ok" True
    terr (LoadedWords wl) msg = 
      assertFailure $ msg ++ " : LoadedSchema " ++ show wl
    terr (Res _ ) msg = 
      assertFailure $ msg ++ " : generic Res"

    tok (Res (NonCompliantList wl)) msg = 
      assertFailure $ msg ++ " : NonCompliantList " ++ show wl
    tok _ _ = assertBool "---" True

    emptyWl = loadWordList (LoadedWords [])
  in

  testGroup "verifica del caricamento della wordlist" 
  [ testCase "wordlist con caratteri non ammessi 1" $
    terr (emptyWl "c1ao m0nd0" )
      "tentativo di inserire `c1ao m0nd0` in []"
  , testCase "wordlist con caratteri non ammessi 2" $
    terr (emptyWl "ciao , mondo" )
      "tentativo di inserire `ciao , mondo` in []"
  , testCase "wordlist con caratteri non ammessi 3" $
    terr (emptyWl "ciao, mondo" )
      "tentativo di inserire `ciao, mondo` in []" 

  ---- casi corretti
  ,  testCase "wordlist con parole ripetute consentite" $
    tok (emptyWl "questo pazzo pazzo mondo")
      "tentativo di inserire `questo pazzo pazzo mondo` in []"
  , testCase "wordlist corretta 1" $
    tok (emptyWl "ciao mondo a b cd" )
      "tentativo di inserire `ciao mondo a b cd` in []"
  , testCase "wordlist corretta 2" $
    tok (loadWordList (LoadedWords ["qui"]) "quo qua" )
      "tentativo di inserire `quo qua` in [qui]"
  ]

searchKeyTests :: TestTree
searchKeyTests = 
  let
    schema :: Schema
    schema = 
      [ [ 'c','o','s','i']
      , [ 'c','i','a','o']
      , [ 'c','o','s','i']
      , [ 'c','i','a','o']
      ]
    words = [ "io", "iso", "osi"]

    tok NoKey _ = assertFailure  " nokey"
    tok (WordsAbsent _) _ = assertFailure  " word absent"
    tok (NonCompliantList _) _ = assertFailure " NonCompliantList"
    tok ( NonCompliantScheme _) _ = assertFailure " NonCompliantScheme"
    tok (Key r) (Key k) = assertEqual "key" r k
    tok _ _ = assertFailure "aiutooooo"
  in
    testCase "chiave" $
      -- searchKey schema words @?= [(0,0),(1,0),(1,2),(2,0),(3,0),(3,2)]
      tok (searchKey schema words) (Key "ccacca")



matchedWordsPosTests :: TestTree
matchedWordsPosTests =
  let
    schema :: Schema
    schema = 
      [ [ 'c','o','s','i']
      , [ 'c','i','a','o']
      , [ 'c','o','s','i']
      , [ 'c','i','a','o']
      ]
    words = [ "io", "iso", "osi"]
    startingPos = 
      [((0,1),"osi"),((0,3),"io"),((0,3),"iso")
      ,((1,1),"io"),((1,1),"iso"),((1,3),"osi")
      ,((2,1),"osi"),((2,3),"io"),((2,3),"iso")
      ,((3,1),"io"),((3,1),"iso"),((3,3),"osi")]
  in
    testCase "posizioni delle parole" $
      matchedWordsPos schema startingPos @?= 
        [ ((0,3), "osi"),((0,2), "osi"),((0,1), "osi") -- osi
        , ((1,3),"io"),((0,3),"io") -- io
        , ((0,1),"iso"),((0,2),"iso"),((0,3),"iso") -- iso
        , ((0,1),"io"),((1,1),"io"),((2,1),"io"),((1,1),"io"),((3,3),"iso"),((2,2),"iso"),((1,1),"iso"),((3,1),"osi"),((2,2),"osi"),((1,3),"osi"),((2,3),"osi"),((2,2),"osi"),((2,1),"osi"),((1,3),"io"),((2,3),"io"),((3,3),"io"),((2,3),"io"),((2,1),"iso"),((2,2),"iso"),((2,3),"iso"),((2,1),"io"),((3,1),"io"),((1,3),"iso"),((2,2),"iso"),((3,1),"iso"),((1,1),"osi"),((2,2),"osi"),((3,3),"osi")
        ]
        
        

positionsTest :: TestTree
positionsTest = 
  let
    s :: Schema
    s = [ ['1','2','3']
        , ['5','6','7']
        ]
  in
    testCase "generazione positions" $
      positions s @?= [(0,0),(0,1),(0,2),(1,0),(1,1),(1,2)]
      
  
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


schemaValueTest :: TestTree
schemaValueTest =
  let
    s :: Schema
    s = [ ['1','2','3','4']
        , ['5','6','7','8']
        , ['a','b','c','d']
        ]
  in
    testGroup "verifca at"
    [ testCase "con schema vuoto torna Nothing" $
        [] `at` (0,0) @?= Nothing
    , testCase "con 0 torna la prima riga" $
        s `at` (0,0) @?= Just '1'
    , testCase "overflow di colonna sulla prima riga" $
        s `at` (0,5) @?= Nothing
    , testCase "lettura di 7" $
        s `at` (1,2) @?= Just '7'
    , testCase "overflow di riga" $
        s `at` (3,0) @?= Nothing
    , testCase "lettura di d" $
        s `at` (2,3) @?= Just 'd'
    ]


startingPositionsTests :: TestTree
startingPositionsTests =
  let
    schema :: Schema
    schema = 
      [ [ 'c','o','s','i']
      , [ 'c','i','a','o']
      , [ 'c','o','s','i']
      , [ 'c','i','a','o']
      ]
    words = [ "io", "iso", "cis", "osi", "si" ]
  in
    testCase "individuazione delle possibili posizioni di partenza delle parole" $
      startingPositions schema words @?= 
        [((0,0),"cis"),((0,1),"osi"),((0,2),"si"),((0,3),"io")
        ,((0,3),"iso"),((1,0),"cis"),((1,1),"io"),((1,1),"iso")
        ,((1,3),"osi"),((2,0),"cis"),((2,1),"osi"),((2,2),"si")
        ,((2,3),"io"),((2,3),"iso"),((3,0),"cis"),((3,1),"io")
        ,((3,1),"iso"),((3,3),"osi")
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
    wcis = "cis"
    wosi = "osi"
    wsi = "si"
    

    tip d w r c = 
      testCase ("impossibile dalla posizione (" ++ show r ++ "," ++ show c ++ ")" ) $
        readWord d w schema [(r,c)] @?= []
  in
    testGroup "Lettura dello schema"
    [ testGroup "verso Nord"
      ( [ testCase "esito positivo da una posizione corretta" $
          readWord Nord word schema [(2,3)] @?= [(1,3),(2,3)]
        , testCase "esito positivo da una posizione corretta2" $
          readWord Nord word schema [(3,1)] @?= [(2,1),(3,1)]
        , testCase "esito positivo da una posizione corretta3" $
          readWord Nord word schema [(1,1)] @?= [(0,1),(1,1)]
        ] 
        ++ map (tip Nord word 0) [0..3]
        ++ map (tip Nord word 2) [0..2]
      )
    , testGroup "verso NordEst"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord NordEst wiso schema [(3,1)] @?= [(1,3),(2,2),(3,1)]
        , testCase "esito positivo da una posizione corretta2" $
            readWord NordEst wcis schema [(2,0)] @?= [(0,2),(1,1),(2,0)]
        ] 
        ++ map (tip NordEst wiso 0) [0..3]
        ++ map (tip NordEst wiso 1) [0..3]
        ++ map (tip NordEst wiso 2) [0..3]
        ++ map (tip NordEst wiso 3) [0,2,3]
      )
    , testGroup "verso Est"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord Est wosi schema [(0,1)] @?= [(0,3),(0,2),(0,1)]
        , testCase "esito positivo da una posizione corretta2" $
            readWord Est wosi schema [(2,1)] @?= [(2,3),(2,2),(2,1)]
        ] 
        ++ map (tip Est wosi 0) [0,2,3]
        ++ map (tip Est wosi 1) [0..3]
        ++ map (tip Est wosi 2) [0,2,3]
        ++ map (tip Est wosi 3) [0..3]
      )
    , testGroup "verso SudEst"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord SudEst wiso schema [(1,1)] @?= [(3,3),(2,2),(1,1)]
        ] 
        ++ map (tip SudEst wiso 0) [0..3]
        ++ map (tip SudEst wiso 1) [0,2,3]
        ++ map (tip SudEst wiso 2) [0..3]
        ++ map (tip SudEst wiso 3) [0..3]
      )
    , testGroup "verso SudOvest"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord SudOvest wsi schema [(0,2)] @?= [(1,1),(0,2)]
        , testCase "esito positivo da una posizione corretta" $
            readWord SudOvest wsi schema [(2,2)] @?= [(3,1),(2,2)]
        ] 
        ++ map (tip SudOvest wsi 0) [0,1,3]
        ++ map (tip SudOvest wsi 1) [0..3]
        ++ map (tip SudOvest wsi 2) [0,1,3]
        ++ map (tip SudOvest wsi 3) [0..3]
      )
    , testGroup "verso NordOvest"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord NordOvest wosi schema [(3,3)] @?= [(1,1),(2,2),(3,3)]
        ] 
        ++ map (tip NordOvest wsi 0) [0..3]
        ++ map (tip NordOvest wsi 1) [0..3]
        ++ map (tip NordOvest wsi 2) [0,1,3]
        ++ map (tip NordOvest wsi 3) [0..2]
      )
    , testGroup "verso Ovest"
      ( [ testCase "esito positivo da una posizione corretta" $
            readWord Ovest wiso schema [(0,3)] @?= [(0,1),(0,2),(0,3)]
        , testCase "esito positivo da una posizione corretta" $
            readWord Ovest wiso schema [(2,3)] @?= [(2,1),(2,2),(2,3)]
        ] 
        ++ map (tip Ovest wiso 0) [0..2]
        ++ map (tip Ovest wiso 1) [0..3]
        ++ map (tip Ovest wiso 2) [0..2]
        ++ map (tip Ovest wiso 3) [0..3]
      )
    ]
