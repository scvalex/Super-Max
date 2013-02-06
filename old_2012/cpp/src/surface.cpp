#include "surface.h"

#include <stdexcept>

using namespace std;

Surface::Surface(int width, int height, bool transparent) :
        original(true)
{
        m_surface = SDL_CreateRGBSurface(SDL_SWSURFACE, width, height, 32,
                                         0, 0, 0, 0);
        if (!m_surface) {
                throw runtime_error("Could not create SDL surface.");
        }
        if (transparent && SDL_SetAlpha(m_surface, SDL_SRCALPHA, 128) == -1) {
                throw runtime_error("Could not alpha SDL sruface.");
        }
        //SDL_FillRect(m_surface, NULL, SDL_MapRGBA(m_surface->format, 0, 0, 0, 0xFF));
}

Surface::Surface(SDL_Surface *newSurface) :
        m_surface(newSurface),
        original(false)
{
}

Surface::~Surface() {
        if (original) {
                SDL_FreeSurface(m_surface);
        }
}

Surface Surface::clip(int x, int y, int w, int h) {
        Surface s(m_surface);
        s.m_clip.x = x;
        s.m_clip.y = y;
        s.m_clip.w = w;
        s.m_clip.h = h;

        return s;
}

SDL_Surface* Surface::sdlSurface() const {
        return m_surface;
}

SDL_Rect* Surface::clip() const {
        return (SDL_Rect*)&m_clip;
}
