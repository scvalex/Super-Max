#include "screen.h"

#include <SDL/SDL.h>
#include <stdexcept>

using namespace std;

Screen::Screen(int width, int height) {
        m_surface = SDL_SetVideoMode(width, height, 32, SDL_SWSURFACE);
        if (!m_surface) {
                throw runtime_error("Could not create screen.");
        }
}

Screen::~Screen() {
}

void Screen::setTitle(const string &title) {
        SDL_WM_SetCaption(title.c_str(), NULL);
}

void Screen::flip() {
        if (SDL_Flip(m_surface) == -1) {
                throw runtime_error("Could not flip screen.");
        }
}

SDL_Surface* Screen::getSurface() const {
        return m_surface;
}
