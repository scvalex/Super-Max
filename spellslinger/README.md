Spellslinger
============

> A game featuring wizards, spells, and monsters

Büild
-----

In addition to the Haskell dependencies (automatically managed by
`cabal-dev`), we need [SDL][sdl] and [SDL_ttf][sdl-ttf].  To install
both, run:

- Debian: `apt-get install libsdl1.2-dev libsdl-ttf2.0-dev`
- Gentoo: `emerge libsdl sdl-ttf`
- Arch:   `pacman -S sdl sdl_ttf`

The build system is driven by the top-level `Makefile` (it's Cabal and
`cabal-dev` underneath).  To build everything, just run:

    make

See the other `make` targets other options.

Rün
---

To build and run the game, just run:

    make run

[sdl]: http://www.libsdl.org/
[sdl-ttf]: http://www.libsdl.org/projects/SDL_ttf/
