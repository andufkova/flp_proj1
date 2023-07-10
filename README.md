# bkg-2-cnf

**Project:** bkg-2-cnf   
**Name:** Aneta Dufková
**Login:** xdufko02
**Year:** 2022

**Description:**
The program converts BKG (context free grammar) to simplified context free grammar or to CNF (chomsky normal form).
The structure is based on the sample project of Turing Machine available in course data storage in WIS (PC-laboratore/Turing_machine).

## Build
- `make` command
- compiled with `ghc` compiler
- binary called `bkg-2-cnf` is created in the root directory

## Run
- `./flp21-fun option [input]`, where 

**option:**  
&emsp;`-i` &emsp;BKG (context free grammar) - this option just parses the input and prints it  
&emsp;`-1` &emsp;BKG (context free grammar) in simplified version
&emsp;`-2` &emsp;BKG in CNF (chomsky normal form) 

**input: (not a mandatory argument)**   
&emsp;` ` &emsp;(empty) Reading BKG from the standard input
&emsp;`filename` &emsp;&nbsp;&nbsp;Reading BKG from the file


## Files
```
.   
├─ doc  
│   └─ README.md
├─ src
│   ├─ Main.hs
│   ├─ Funcs.hs
│   ├─ ParseInput.hs
│   └─ DataTypes.hs
├─ test
│   ├─ test1.in
│   ├─ test1.out
│   ├─ test1_2.out
│   ├─ test2.in
│   ├─ test2.out
│   └─ test2_2.out
├─ test.sh
└─ Makefile
```

## Makefile options
- `default`: compiles the program, creates a binary in the root directory
- `zip`: creates a zip file ready for uploading
- `clear`: deletes all the files created during a compilation, testing
- `run_test`: runs test.sh script
- `run`: runs the program with option -2 and test file number 1

## Testing
Testing script can be run as `./test.sh` without any argument. It runs the program with test files from the test folder.
- name.in = input
- name.out = output for option -1 (simplified BKG)
- name\_2.out = output for option -2 (BKG in CNF)
