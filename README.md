# crucipuzzle101101

- autore: Costantino Giuliodori
- matricola: 101101

## build

la struttura e' creata utilizzando `stack` e `hpack` partendo da `package.yaml` in modo da allineare `cabal` automaticamente.

## eseguire il programma

Il programma puo' essere avviato in due modi, o passandogli direttamente lo schema e la wordlist come argomenti
`stack run -- schema.txt wordlist.txt`

dove sia lo schema e' la matrice di lettere, mentre la wordlist e' una lista di parole da individuare nello schema, ciascuna su una riga diversa.

oppure senza argomenti: il programma partira' in modalita' interattiva, chiedendo prima l'inserimento dello schema e poi della wordlist.
`stack run`

E' disponibile una suite di test che possono essere lanciati con `stack test`.

## struttura del programma

Il programma e' diviso in tre directory e tre file sorgente:

- app/Main.hs : la parte che si occupa dell'avvio del programma e le funzioni che interagiscono con l'I/O
- src/Lib.hs : l'insieme delle funzioni pure utilizzate da Main.hs che non hanno a che fare con l'I/O
- test/spec.hs : i test unitari per le funzioni contenute in Lib.hs

### strutture dati utilizzate

- `Status` lo stato complessivo del programma
  in qualsiasi momento si guardi il programma (dopo la valutazione dei parametri)
  si potra' trovarlo con:

  - uno schema caricato (anche vuoto)
  - l'elenco delle parole caricato
  - con un risultato
  
- `Result` il risultato dell'elaborazione che puo' essere:
  - errore per lista non corretta
  - errore per alcune parole nell'elenco mancanti
  - errore per assenza della chiave
  - chiave

### flusso del programma

La soluzione e' stata individuata utilizzando il principio di moltiplicazione.

Si carica lo schema, se questo e' corretto si procede con il caricamento dell'elenco di parole.
Se anche l'elenco e' accettabile il programma:

- percorre tutte le posizioni cercando da quali possono partire le parole dell'elenco (se alla posizione x,y corrisponde l'iniziale della parola) (in `startingPositions`)
- per ogni posizione individuata cerca, possibilmente in parallelo, in tutte le direzioni se lo schema contiene la parola che inizia a quella posizione, registrando le posizioni coperte dalla parola (in `readWord`)
- verifica che non ci siano parole nell'elenco che non sono state trovate (in `searchkey`)
- estrae le lettere non coperte da nessuna parola (chiave) (in `searchkey`)

### accorgimenti utilizzati - librerie

Come ottimizzazione dell'algoritmo e' stato introdotto un parallelismo semi esplicito tramite l'uso degli `spark` (spark computation) generati tramite una strategia di esecuzione che disaccoppia la funzione dal modo in cui viene valutata.

La concorrenza e' quindi ottenuta proponendo la creazione di thread.

Ad esempio lanciando l'esecuzione con `stack run -- schema.txt elenco.txt +RTS -N4 -s ` si ottiene l'evidenza dell'esecuzione parallela, che viene ignorata se lo spazio del problema e' piccolo.

