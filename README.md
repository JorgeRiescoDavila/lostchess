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
### Core
* ✔ board representation and constants definitions
* ✔ move generator
* ✔ add move to list
* ✔ move maker
* ✔ move unmaker
* ✔ attacked square
* ✔ hash position 
### Tests
* ✔ unit tests
* ✔ perft
* test hash
### Utilities and I/O
* ✔ board writer
* ✔ moves writer
* ✔ fen parser
### Search engine
* ✔ static analyzer
* ✔ min-max
* get principal line
* ✔ alpha-beta
* sorted alpha-beta and killers
* ✔ search handler
* extensions
* transposition tables
### API
