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

Image::~Image() {
        SDL_FreeSurface(m_surface);
}

void Image::drawOnto(const IsSurface &canvas) {
        drawOntoAt(canvas, 0, 0);
}

void Image::drawOntoAt(const IsSurface &canvas, int x, int y) {
        SDL_Rect offset;
        offset.x = x;
        offset.y = y;

        int result = SDL_BlitSurface(getSurface(), NULL, canvas.getSurface(), &offset);
        if (result == -1) {
                throw runtime_error(string("Could not blit surface: ") +
                                    SDL_GetError());
        } else if (result == -2) {
                cout << "BlitSurface returned -2; see docs" << endl;
        }
}

SDL_Surface* Image::getSurface() const{
        return m_surface;
}
