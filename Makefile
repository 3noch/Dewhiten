all:
	ghc -o dewhiten --make Dewhiten

clean:
	rm *.o *.hi dewhiten
