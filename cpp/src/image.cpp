#include "image.h"

#include <iostream>
#include <SDL/SDL.h>
#include <SDL/SDL_image.h>
#include <stdexcept>

using namespace std;

Image::Image(const string &filename) {
        m_surface = IMG_LoadPNG_RW(SDL_RWFromFile("data/hello.png", "rb"));
        if (!m_surface) {
                throw runtime_error("Could not load " + filename + ": " + IMG_GetError());
        }
}

Image::~Image() {
        SDL_FreeSurface(m_surface);
}

void Image::drawOnto(const IsSurface &canvas) {
        int result = SDL_BlitSurface(getSurface(), NULL, canvas.getSurface(), NULL);
        if (result == -1) {
                throw runtime_error(string("Could not blit surface: ") + SDL_GetError());
        } else if (result == -2) {
                cout << "BlitSurface returned -2; see docs" << endl;
        }
}

SDL_Surface* Image::getSurface() const{
        return m_surface;
}
