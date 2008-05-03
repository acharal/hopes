
BUILDARGS=

SETUPHS=./Setup.hs
#SETUP=runhaskell $(SETUPHS)
SETUP=./setup
RM=rm
HC=ghc
DARCS=darcs
HOPEVERSION=0.0.5
HOPEBALL=hopes-$(HOPEVERSION).tar.gz

TMPDISTDIR=/tmp/hopesdist

all: build

build: build-stamp

build-stamp: config
	$(SETUP) build $(BUILDARGS)

config: .setup-config

setup:
	$(HC) --make $(SETUPHS) -o $(SETUP)

.setup-config: setup
	$(SETUP) configure

doc: haddock

haddock: config
	$(SETUP) haddock

install: build-stamp
	$(SETUP) install

clean: setup
	$(SETUP) clean
	$(RM) -rf dist
	$(RM) -f *.hi *.o

maintainer-clean: clean
	$(RM) -f .setup-config
	$(RM) -f $(SETUP)

AUTHORS: 
	echo "Angelos Charalambidis <a.charalambidis@di.uoa.gr>" > AUTHORS

$(HOPEBALL):
	darcs dist --dist-name hope-$(HOPEVERSION)

dist: $(HOPEBALL)
	rm -rf $(TMPDISTDIR)
	mkdir $(TMPDISTDIR)
	mv $(HOPEBALL) $(TMPDISTDIR)
	@echo "Hope source tarball built: $(TMPDISTDIR)/$(HOPEBALL)" 

deb: dist
	cd $(TMPDISTDIR) && ln -s $(HOPEBALL) haskell-hope.orig.tar.gz
	cd $(TMPDISTDIR) && tar zxvf $(HOPEBALL)
	mv $(TMPDISTDIR)/hope $(TMPDISTDIR)/haskell-hope-$(HOPEVERSION)
	cd $(TMPDISTDIR)/haskell-hope-$(HOPEVERSION) && debuild
	rm -rf $(TMPDISTDIR)

.PHONY: all configure build install dist src-dist darcs-dist clean maintainer-clean

