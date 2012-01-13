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

int Image::getWidth() const {
        return m_surface->w;
}

int Image::getHeight() const {
        return m_surface->h;
}

void Image::drawOnto(const IsSurface &canvas, int x, int y) {
        SDL_Rect offset;
        offset.x = x;
        offset.y = y;

        drawInternal(m_surface, canvas.getSurface(), NULL, &offset);
}

void Image::drawOntoClip(const IsSurface &canvas, int dx, int dy,
                         int sx, int sy, int sw, int sh)
{
        SDL_Rect offset;
        offset.x = dx;
        offset.y = dy;

        SDL_Rect clip;
        clip.x = sx;
        clip.y = sy;
        clip.w = sw;
        clip.h = sh;

        drawInternal(m_surface, canvas.getSurface(), &clip, &offset);
}

void Image::drawInternal(SDL_Surface *img, SDL_Surface *canvas,
                         SDL_Rect *clip, SDL_Rect *offset)
{
        int result = SDL_BlitSurface(img, clip, canvas, offset);
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
