
BUILDARGS=

SETUPFILE=./Setup.hs
SETUP=runhaskell $(SETUPFILE)
RM=rm
GHC=ghc
DARCS=darcs

all: build

build: configure
	$(SETUP) build $(BUILDARGS)

clean:
	$(SETUP) clean
	$(RM) -rf dist
	$(RM) -f *.hi *.o

configure: .setup-config

.setup-config:
	$(SETUP) configure

doc:
	$(SETUP) haddock

dist: build
	$(SETUP) sdist

darcs-dist: 
	$(DARCS) dist

install: build
	$(SETUP) install

buildsetup:  $(SETUPFILE)
	$(GHC) --make $(SETUPFILE) -o setup

