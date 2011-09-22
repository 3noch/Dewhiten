PREFIX = /usr/local
BIN = $(PREFIX)/bin
EXE = dewhiten

MAIN = Dewhiten.hs

all: $(EXE)

install: all
	install dewhiten $(BIN)/$(EXE)

clean:
	rm *.o *.hi $(EXE)

$(EXE): $(wildcard *.hs)
	ghc -o $(EXE) --make $(MAIN)
