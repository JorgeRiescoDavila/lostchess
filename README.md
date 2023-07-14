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
It uses 0x88 board representation, pseudo-legal move generator, 32-bit Zobrist hash, perft with transposition table and supports FEN.

Static evaluation uses piece material and piece-square tables for start and end positions.

Search is done with an alpha-beta prune in negamax form, sorting captures by MVV-LVA and sorting quiet moves by killers. It also has quiescience search on leaf nodes, time based iterative deepening, checkmate distance handler and returns the principal line.

## PerfT results
Results for some positions, also checking that hash and fen are unafected after going through the full tree.
```
 starting position
          20          20 T
         400         400 T
        8902        8902 T
      197281      197281 T
     4865609     4865609 T
unchanged hash           T
unchanged fen            T
 kiwipete
          48          48 T
        2039        2039 T
       97862       97862 T
     4085603     4085603 T
unchanged hash           T
unchanged fen            T
 perft position 3
          14          14 T
         191         191 T
        2812        2812 T
       43238       43238 T
      674624      674624 T
    11030083    11030083 T
unchanged hash           T
unchanged fen            T
 perft position 4
           6           6 T
         264         264 T
        9467        9467 T
      422333      422333 T
unchanged hash           T
unchanged fen            T
 perft position 5
          44          44 T
        1486        1486 T
       62379       62379 T
     2103487     2103487 T
unchanged hash           T
unchanged fen            T
 time:    6.67187500
 TT entrys/size         2783        8192
 TT skiped nodes       696299
```
