
BUILDARGS=

SETUPHS=./Setup.hs
#SETUP=runhaskell $(SETUPHS)
SETUP=./setup
CABAL=cabal
HC=ghc
DARCS=darcs
HOPEVERSION=0.0.5
HOPEBALL=hopes-$(HOPEVERSION).tar.gz

TMPDISTDIR=/tmp/hopesdist

all: build

build: build-stamp

build-stamp: config
	$(CABAL) build $(BUILDARGS)

config: .setup-config

setup:
	$(HC) --make $(SETUPHS) -o $(SETUP)

.setup-config:
	$(CABAL) configure

doc: haddock

man:
	cd docs && $(MAKE) man

haddock: config
	$(CABAL) haddock --executables

install-binary: build-stamp
	$(CABAL) install

install: install-binary install-docs

install-docs: man

clean:
	$(CABAL) clean
	rm -rf dist
	rm -f *.hi *.o

full-clean: clean
	rm $(SETUP)

maintainer-clean: clean
	rm -f .setup-config
	rm -f $(SETUP)
	rm -f Setup.hi Setup.o

AUTHORS: 
	echo "Angelos Charalambidis <a.charalambidis@di.uoa.gr>" > AUTHORS

$(HOPEBALL):
	darcs dist --dist-name hopes-$(HOPEVERSION)

dist: $(HOPEBALL)
	rm -rf $(TMPDISTDIR)
	mkdir $(TMPDISTDIR)
	mv $(HOPEBALL) $(TMPDISTDIR)
	@echo "Hope source tarball built: $(TMPDISTDIR)/$(HOPEBALL)" 

deb: dist
	cd $(TMPDISTDIR) && tar zxvf $(HOPEBALL)
	cd $(TMPDISTDIR)/hopes-$(HOPEVERSION) && ln -s release/debian .
	cd $(TMPDISTDIR)/hopes-$(HOPEVERSION) && debuild -us -uc
	cp $(TMPDISTDIR)/*.deb .
	rm -rf $(TMPDISTDIR)

rpm: dist
	cd $(TMPDISTDIR) && tar zxvf $(HOPEBALL)
	cd $(TMPDISTDIR)/hopes-$(HOPEVERSION) && ln -s release/rpm/hopes.spec .
	rpmbuild
	# rm -rf $(TMPDISTDIR)

.PHONY: all configure build install dist src-dist darcs-dist clean maintainer-clean

