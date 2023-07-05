# Lostchess
A rewrite of my lost chess program

## Installation
Compile using https://www.mingw-w64.org/ with the commands:
```
gfortran lostchess.f90 -o lostchess.exe
start lostchess.exe
```
Or download the lostchess.exe for 64bits Windows

##Roadmap
* king history
* depth based to time based search
* piece tables for finals
* test finals
* debug quiescient
* debug score/sort moves
###Could have
* handle promotions on read move
* hash empty piece = 0
* RNG for hash instead of abssin
* piece list
* get principal line
* test hash tables
* transposition tables
* 50 move rule on fen



