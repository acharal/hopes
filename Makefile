RUNHASKELL=runhaskell
RM=rm

build:
	$(RUNHASKELL) ./Setup.hs build

clean:
	$(RM) -r -f src/*.hi src/*.o src/*/*.o src/*/*.hi
