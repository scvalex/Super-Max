all: build

.PHONY: build test doc clean p bench

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure --enable-tests

build: setup.data
	ocaml setup.ml -build

test: build
	ocaml setup.ml -test

doc: build
	ocaml setup.ml -doc

clean:
	-ocaml setup.ml -clean
	rm -f myocamlbuild.ml setup.ml setup.data

p:
	permamake.sh Makefile \
	             _oasis \
	             bin/*.ml*
