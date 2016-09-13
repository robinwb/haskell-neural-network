FILES=Network.hs Rob.hs
OPTS=-O9 #-debug -rtsopts -with-rtsopts=-K150000000
LIBS=

MAIN=main.hs
EXEC=run
COMP=ghc

all: $(EXEC)
$(EXEC): $(MAIN) $(FILES)
	$(COMP) $(MAIN) -o $(EXEC) $(OPTS) $(LIBS)
clean: 
	find . -name '*.o' -print0 | xargs -0 rm
	find . -name '*.hi' -print0 | xargs -0 rm
	rm $(EXEC)
