
all: build

clean:
	rm -rf dist
	rm -f SuperMax

build:
	cabal build ; cp dist/build/SuperMax/SuperMax ./

run: build
	./SuperMax
