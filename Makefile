
BUILDARGS=

SETUPFILE=./Setup.hs
SETUP=runhaskell $(SETUPFILE)
RM=rm
GHC=ghc

all: build

build: configure
	$(SETUP) build $(BUILDARGS)

clean:
	$(SETUP) clean

configure: .setup-config

.setup-config:
	$(SETUP) configure

doc:
	$(SETUP) haddock


dist: build
	$(SETUP) sdist

install: build
	$(SETUP) install

setup:  $(SETUPFILE)
	$(GHC) --make $(SETUPFILE) -o setup

