PREFIX = /usr/local
BIN = $(PREFIX)/bin
EXE = dewhiten

MAIN = Dewhiten.hs

all: $(EXE)

install: all
	install dewhiten $(BIN)/$(EXE)

clean:
	rm -f *.o *.hi $(EXE)

$(EXE): $(wildcard *.hs)
	ghc -O2 -o $(EXE) --make $(MAIN)
