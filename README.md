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
------------

The following programs and libraries are necessary to build Super Max:

### [cmake](http://www.cmake.org/)

The minimum required version is `2.6`.

>  Debian: `apt-get install cmake`
>
>  Gentoo: `emerge cmake`

### [Chipmunk](http://chipmunk-physics.net/) (on [GitHub](https://github.com/slembcke/Chipmunk-Physics))

We use version `6.0.X`, which is not available in either Gentoo or
Debian, so you need to install it manually:

     get the source from the Chipmunk website or GitHub
     % cd Chipmunk-Physics
     % mkdir build
     % cd build
     % cmake ..
     % make
     % sudo make install

### [SDL](http://www.libsdl.org/)

There shouldn't be any problems with recentish versions.

> Debian: apt-get install libsdl1.2-dev libsdl-image1.2-dev libsdl-mixer1.2-dev libsdl-ttf2.0-dev
>
> Gentoo: emerge libsdl sdl-image sdl-mixer sdl-ttf


The fine print
--------------

Trademarks property of their respective owners.  Code owned by the
commiter.  Super Max is licensed under the GNU General Public License
Version 3 (see `LICENSE`) for details.
