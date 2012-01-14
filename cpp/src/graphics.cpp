#include "graphics.h"

#include <cassert>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <stdexcept>

using namespace std;

bool Graphics::initialised = false;

Graphics::Graphics() {
        assert(!initialised);
        initialised = true;

        if (SDL_Init(SDL_INIT_EVERYTHING) == -1) {
                throw runtime_error("Could not initialise SDL");
        }

        if (!IMG_Init(IMG_INIT_PNG)) {
                throw runtime_error("Could not initialise SDL_image");
        }

        if (TTF_Init() == -1) {
                throw runtime_error("Could not initialise SDL_ttf");
        }
}

Graphics::~Graphics() {
        TTF_Quit();
        IMG_Quit();
        SDL_Quit();
}
