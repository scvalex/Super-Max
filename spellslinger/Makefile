BIN := dist/build/spellslinger/spellslinger
REDIST := spellslinger-redist

all: build

.PHONY: all build dist install clean doc p ghci

build: dist/setup-config
	cabal-dev build

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
	cabal-dev clean
	rm -rf cabal-dev/ spellslinger-redist/

dist/setup-config: spellslinger.cabal
# If you don't have all the necessary packages installed on the first
# run, run `cabal-dev install`.
	cabal-dev configure || cabal-dev install

doc: build
	cabal-dev haddock

p:
	permamake.sh $(shell find src/ -name '*.hs') \
                     $(shell find engine/ -name '*.hs') \
	             *.cabal \
	             Makefile

run: build
	$(BIN)

ghci: build
	cabal-dev ghci
