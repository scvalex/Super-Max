all: build

.PHONY: build test doc clean p bench

setup.ml: _oasis
	oasis setup

setup.data: setup.ml
	ocaml setup.ml -configure --enable-tests

build: setup.data
	ocaml setup.ml -build -cflags -annot

test: build
	ocaml setup.ml -test

doc: build
	ocaml setup.ml -doc

clean:
	-ocaml setup.ml -clean
	rm -f setup.ml setup.data lib/META

p:
	permamake.sh Makefile \
	             myocamlbuild.ml \
	             _oasis \
	             lib/*.ml* \
	             bin/*.ml*
