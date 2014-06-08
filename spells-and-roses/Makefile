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
	misc/check_all.sh

doc: build
	ocaml setup.ml -doc

clean:
	-ocaml setup.ml -clean
	rm -f setup.ml setup.data */META
	rm -f */*.mllib */*.mldylib */*.clib */*.mlpack

p:
	permamake.sh Makefile \
	             myocamlbuild.ml \
	             _oasis \
	             super_max/*.ml* \
	             spells_and_roses/*.ml* \
	             bin/*.ml* \
	             tangled/*.ml*
