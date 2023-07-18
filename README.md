# Lostchess
A rewrite of my lost chess program

## Installation
Compile using https://www.mingw-w64.org/ with the commands:
```
gfortran lostchess.f90 -o lostchess.exe
start lostchess.exe
```
Or download the lostchess.exe for 64bits Windows

## Technical details
### Generation
* Board representation 0x88
* Hash function Zobrist 64-bit
* FEN support
* PerfT with Transposition Table

### Evaluation
* Material
* Piece-square tables

### Search
* Alpha-beta prune in negamax form
* Iterative deepening with aspiration window
* Move sorting by Killers, PV, MVV-LVA and Heuristics
* Quiescience search
* Checkmate distance handler
* Transposition Table
* Principal Variation Table
* Principal Variation Search
* Null move forward pruning

## PerfT results
Perft results with transposition table info.
```
 starting position
          20          20 T
         400         400 T
        8902        8902 T
      197281      197281 T
     4865609     4865609 T
   119060324   119060324 T
 time:    17.2656250
 TT entrys/size        65536       65536
 TT skiped nodes     43550632
 
 kiwipete
          48          48 T
        2039        2039 T
       97862       97862 T
     4085603     4085603 T
   193690690   193690690 T
 time:    27.9375000
 TT entrys/size        65536       65536
 TT skiped nodes     60225931
```

## Search results
Search results for starting position and kiwipete
```
score    depth       nodes      qnodes        time move          fhf          fh ratio    TTwrites     TTreads   TTentries pv
50           1          21          21           0 b1c3            0           0  0.00           1           0           1 b1c3 
 0           2          42          46           0 b1c3           19          19  0.95          22           0          21 b1c3  b8c6 
50           3         467         467           0 b1c3           37          38  0.95          82           0          60 b1c3  b8c6  g1f3 
-5           4         689         816           0 b1c3          199         207  0.96         319          18         261 b1c3  b8c6  g1f3  g8f6 
40           5        6016        6341          15 b1c3          420         433  0.97         999          90         719 b1c3  b8c6  g1f3  g8f6  d2d4 
-5           6       61741       74132         125 d2d4         5993        6404  0.94        9870        1262        7336 d2d4  d7d5  g1f3  g8f6  b1c3  b8c6 
55           7       90138      114660         187 g1f3        13057       13985  0.93       27094        4421       17733 g1f3  d7d5  e2e3  c8g4  b1c3  g8f6  d2d4 
 0           8      289372      399797         640 g1f3        34946       38604  0.91       75647       10770       41515 g1f3  d7d5  b1c3  b8c6  d2d4  g8f6  c1e3  e7e6 

70           1          48        1389          15 e2a6            0           0  0.00           1           0           1 e2a6 
70           2          86        1574           0 e2a6           44          47  0.92          50           0          49 e2a6  b4c3 
50           3        2243        4302           0 e2a6           75          85  0.87         185           0         135 e2a6  b4c3  d2c3 
50           4        3475       20322          31 e2a6         1727        1768  0.98        2051         522        1838 e2a6  b4c3  d2c3  h3g2 
50           5       47776       88136         140 e2a6         3035        3140  0.97        6676        1205        4756 e2a6  b4c3  d2c3  h3g2  f3g2 
50           6       46551      412421         593 e2a6        10488       10978  0.96       18972        2387       14407 e2a6  b4c3  d2c3  h3g2  f3g2  e6d5 
30           7      422375      894281        1281 e2a6        30531       32182  0.95       63143        7758       37438 e2a6  b4c3  d2c3  e6d5  e5g4  h3g2  f3g2 
```

## Roadmap
* Improve time controller
* NNUE
* UCI
