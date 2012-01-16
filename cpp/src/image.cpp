#include "image.h"

#include <iostream>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <stdexcept>

using namespace std;

Image::Image(const string &filename) {
        SDL_Surface *aux = IMG_Load(filename.c_str());
        if (!aux) {
                throw runtime_error("Could not load " + filename + ": " + IMG_GetError());
        }

        m_surface = SDL_DisplayFormatAlpha(aux);
        SDL_FreeSurface(aux);
        if (!m_surface) {
                throw runtime_error("Could not optimise " + filename + ": " + IMG_GetError());
        }
}

Image::Image(SDL_Surface *surface) :
        m_surface(surface)
{
}

Image::~Image() {
        SDL_FreeSurface(m_surface);
}

int Image::width() const {
        return m_surface->w;
}

int Image::height() const {
        return m_surface->h;
}

SDL_Surface* Image::surface() const{
        return m_surface;
}
