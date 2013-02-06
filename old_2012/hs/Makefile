
all: build

clean:
	cabal clean
	rm -f SuperMax

build:
	cabal build
	cp dist/build/SuperMax/SuperMax ./

run: build
	./SuperMax

configure:
	cabal configure
