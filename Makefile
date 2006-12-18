RUNHASKELL=runhaskell
RM=rm

all: build

build:
	$(RUNHASKELL) ./Setup.hs build

clean:
	$(RUNHASKELL) ./Setup.hs clean
