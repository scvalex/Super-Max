BIN := dist/build/spellslinger/spellslinger
REDIST := spellslinger-redist
CABAL := $(shell cabal-dev --version > /dev/null && echo cabal-dev || echo cabal)

all: build

.PHONY: all build dist install clean doc p ghci

build: dist/setup-config
	$(CABAL) build

redist: build
	rm -rf $(REDIST)/
	mkdir -p $(REDIST)/
	cp $(BIN) $(REDIST)/spellslinger-x64
	cp $(shell objdump -p $(REDIST)/spellslinger-x64 | grep NEEDED | grep -i sdl | awk '{ printf "/usr/lib64/%s\n", $$2 }') $(REDIST)/
	cp ./spellslinger $(REDIST)/
	cp -r r/ $(REDIST)/
	tar -cjvf spellslinger-redist.tar.bz2 $(REDIST)/

install: build
	cabal install

clean:
	$(CABAL) clean
	rm -rf cabal-dev/ spellslinger-redist/ spellslinger-redist.tar.bz2

dist/setup-config: spellslinger.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	$(CABAL) configure || $(CABAL) install

doc: build
	$(CABAL) haddock

p:
	permamake.sh $(shell find src/ -name '*.hs') \
                     $(shell find engine/ -name '*.hs') \
	             *.cabal \
	             Makefile

run: build
	$(BIN)

ghci: build
	cabal-dev ghci
