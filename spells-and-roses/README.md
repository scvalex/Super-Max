Spells and Roses
================

> A game with fireballs and romance!

Dependencies
------------

* [OCaml](http://ocaml.org/)
* [Opam](http://opam.ocaml.org/)
* [SDL 2.0](http://libsdl.org/)
* [SDL_ttf 2.0](http://www.libsdl.org/projects/SDL_ttf/)
* [SDL_image 2.0](http://www.libsdl.org/projects/SDL_image/)
* [Core](https://github.com/janestreet/core):

~~~~
opam install core
~~~~

* [Oasis](https://github.com/ocaml/oasis)

~~~~
opam install oasis
~~~~

* [OCamlSDL2](https://github.com/blue-prawn/OCamlSDL2):

~~~~
git clone https://github.com/blue-prawn/OCamlSDL2.git
cd OCamlSDL2/src
cp Makefile.config{.unix,}
make findreinstall
make findinstall_h
~~~~

* [OCamlSDL2_TTF](https://github.com/blue-prawn/OCamlSDL2_TTF):

~~~~
git clone https://github.com/blue-prawn/OCamlSDL2_TTF.git
cd OCamlSDL2_TTF/src
make reinstall
~~~~

* [OCamlSDL2_Image](https://github.com/blue-prawn/OCamlSDL2_Image):

~~~~
git clone https://github.com/blue-prawn/OCamlSDL2_Image.git
cd OCamlSDL2_Image/src
make reinstall
~~~~

Build and run
-------------

    make
    ./spells_and_roses.native help -r
