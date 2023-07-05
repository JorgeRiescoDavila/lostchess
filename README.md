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
* depth based to time based search
* test hash tables
* handle promotions on read move
* test finals
###Could have
* hash empty piece = 0
* RNG for hash instead of abssin
* piece list
* get principal line
* transposition tables
* 50 move rule on fen



