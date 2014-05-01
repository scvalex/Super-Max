Spells and Roses
================

> A game with fireballs and romance!

Dependencies
------------

* [OCaml](http://ocaml.org/)
* [Opam](http://opam.ocaml.org/)
* [SDL 2.0](http://libsdl.org/)
* [SDL_ttf 2.0](http://www.libsdl.org/projects/SDL_ttf/)
* [Core](https://github.com/janestreet/core):

~~~~
opam install core
~~~~

* [OCamlSDL2](https://github.com/blue-prawn/OCamlSDL2):

~~~~
git clone https://github.com/blue-prawn/OCamlSDL2
cd OCamlSDL2/src
cp Makefile.config{.unix,}
make findreinstall
make findinstall_h
~~~~

* [OCamlSDL2_TTF](https://github.com/blue-prawn/OCamlSDL2_TTF):

````
git clone https://github.com/blue-prawn/OCamlSDL2_TTF
cd OCamlSDL2_TTF/src
make install
~~~~

Build
-----

    make
