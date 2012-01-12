#include "graphics.h"

#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <stdexcept>

using namespace std;

Graphics::Graphics() {
        if (SDL_Init(SDL_INIT_EVERYTHING) == -1) {
                throw runtime_error("Could not initialise SDL");
        }

        if (!IMG_Init(IMG_INIT_PNG)) {
                throw runtime_error("Could not initialise SDL_image");
        }
}

Graphics::~Graphics() {
        IMG_Quit();
        SDL_Quit();
}

void Graphics::delay(int milli) {
        SDL_Delay(milli);
}
