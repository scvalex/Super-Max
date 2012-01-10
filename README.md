Super Max
=========

Development
-----------

Ultimately, the project is built by `cmake` (see `CMakeLists.txt`),
but development is `Makefile`-driven.

To build everything, run:

    % make

To run the game:

    $ make run

To clean all temporary files:

    $ make clean

Dependencies
============

The following programs and libraries are necessary to build Super Max:

* [cmake](http://www.cmake.org/)

    >  Debian: `apt-get install cmake`
    >
    >  Gentoo: `emerge cmake`

* [Chipmunk](http://chipmunk-physics.net/) (on
  [GitHub](https://github.com/slembcke/Chipmunk-Physics))

    > Debian: `apt-get install chipmunk-dev`
    >
    > Gentoo: manual installation
