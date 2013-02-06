all: build

.PHONY: all build dist install clean doc p ghci

build: dist/setup-config
	rm -rf _site _cache
	cabal-dev build

dist:
	cabal-dev sdist

install: build
	cabal install

clean:
	cabal-dev clean
	rm -rf cabal-dev/

dist/setup-config: blind-horror.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure || cabal-dev install

doc: build
	cabal-dev haddock

p:
	permamake.sh $(shell find src/ -name '*.hs') \
	             *.cabal \
	             Makefile

ghci: build
	cabal-dev ghci
