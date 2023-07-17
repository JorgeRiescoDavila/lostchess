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
It uses 0x88 board representation, pseudo-legal move generator, 64-bit Zobrist hash, perft with transposition table and supports FEN.

Static evaluation uses piece material and piece-square tables for start and end positions.

Search is done with an alpha-beta prune in negamax form, sorting by killers, mvv_lva and heuristics. It has quiescience search on leaf nodes, time based iterative deepening, checkmate distance handler and returns the principal line.

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
Starting position, quiescent nodes are not shown.
```
score dp       nodes  time  move       fhf/fh   ratio     TTw    TTr entry pv
   50  1          20     0 b1c3        0       0 0.00       1      0     1 pv b1c3
    0  2          74     0 b1c3       17      18 0.89      22      0    21 pv b1c3 b8c6
   50  3         484     0 b1c3       55      56 0.96     101      0    79 pv b1c3 b8c6 g1f3
    0  4        1394     0 b1c3      411     434 0.94     598     65   495 pv b1c3 b8c6 g1f3 g8f6
   40  5        9491    15 b1c3     1182    1236 0.96    2235    325  1614 pv b1c3 b8c6 g1f3 g8f6 d2d4
    0  6       28307    93 b1c3     8410    8958 0.94   12489   5277  9713 pv b1c3 b8c6 g1f3 d7d5 d2d4 g8f6
   55  7      212791   421 g1f3    30626   32754 0.94   53603  15476 32719 pv g1f3 d7d5 e2e3 c8g4 d2d4 b8c6 b1c3
    0  8     1026791  3234 b1c3   245999  264418 0.93  357834  98100 64846 pv b1c3 d7d5 d2d4 b8c6 c1f4 e7e6 g1f3 g8f6
```

## Roadmap
* Use PV to score moves
* Nullmove forward prune
* Aspiration window
* Improve time controller
* NNUE
* UCI
