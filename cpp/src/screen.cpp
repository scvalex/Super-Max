#include "screen.h"

#include <cassert>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <SDL/SDL_ttf.h>
#include <stdexcept>

using namespace std;

Screen* Screen::instance = NULL;

Screen::Screen(int width, int height) {
        assert(!instance);
        instance = this;

        // Initialise SDL&co
        if (SDL_Init(SDL_INIT_EVERYTHING) == -1) {
                throw runtime_error("Could not initialise SDL");
        }

        if (!IMG_Init(IMG_INIT_PNG)) {
                throw runtime_error("Could not initialise SDL_image");
        }

        if (TTF_Init() == -1) {
                throw runtime_error("Could not initialise SDL_ttf");
        }

        // Show screen
        m_surface = SDL_SetVideoMode(width, height, 32, SDL_SWSURFACE);
        if (!m_surface) {
                throw runtime_error("Could not create screen.");
        }
}

Screen::~Screen() {
        TTF_Quit();
        IMG_Quit();
        SDL_Quit();
}

Screen& Screen::screen() {
        return *instance;
}

void Screen::setTitle(const string &title) {
        SDL_WM_SetCaption(title.c_str(), NULL);
}

void Screen::flip() {
        if (SDL_Flip(m_surface) == -1) {
                throw runtime_error("Could not flip screen.");
        }
}

SDL_Surface* Screen::sdlSurface() const {
        return m_surface;
}
