# FLP: bkg-2-cnf
# Aneta Dufkova (xdufko02)
# 2022

TARGET=flp21-fun
SOURCE_FILES=src/Main.hs src/ParseInput.hs src/DataTypes.hs src/Funcs.hs
EXECUTABLE=src/*.hi src/*.o
ZIP=flp-fun-xdufko02.zip
FOLDERS=src/*.* doc/*.* test/*.*
OTHER_FILES=test.sh

default:
	ghc ${SOURCE_FILES} -Wall -o ${TARGET}

zip:
	zip ${ZIP} ${FOLDERS} ${OTHER_FILES} Makefile ${TARGET}

clear:
	rm -rf *.hi *.o *.tst ${EXECUTABLE} ${ZIP}

run_test:
	./test.sh

run:
	./flp21-fun -1 test/test1.in




