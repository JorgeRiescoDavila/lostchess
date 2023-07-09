# Lostchess
A rewrite of my lost chess program

## Installation
Compile using https://www.mingw-w64.org/ with the commands:
```
gfortran lostchess.f90 -o lostchess.exe
start lostchess.exe
```
Or download the lostchess.exe for 64bits Windows

## Roadmap
* rewrite generator to piece list
* rewrite player_handler to pseudolegals
* rewrite alpha-beta to handle pseudolegals
* finish fen
* handle promotions on read move
* depth based to time based search
* rewrite quescience
* tapered eval
* transposition tables
* RNG for hash
* get principal line




