SETUP=runhaskell ./Setup.hs
RM=rm

all: build

build: .setup-config
	$(SETUP) build

clean:
	$(SETUP) clean

.setup-config:
	$(SETUP) configure

doc:
	$(SETUP) haddock


dist: build
	$(SETUP) sdist


install: build
	$(SETUP) install
