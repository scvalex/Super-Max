#ifndef SURFACE_H
#define SURFACE_H

#include "issurface.h"
#include <SDL/SDL.h>

class Surface : public IsSurface {
public:
        Surface(int width, int height, bool transparent = false);
        Surface(SDL_Surface *newSurface);
        virtual ~Surface();

        Surface clip(int x, int y, int w, int h);

        SDL_Surface* surface() const;
        SDL_Rect* clip() const;

private:
        SDL_Surface *m_surface;
        bool original;
        SDL_Rect m_clip;
};

#endif
