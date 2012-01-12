#ifndef SCREEN_H
#define SCREEN_H

#include "issurface.h"

class SDL_Surface;

class Screen : public IsSurface {
public:
        Screen(int width, int height);
        virtual ~Screen();

        void flip();

        SDL_Surface* getSurface() const;

private:
        SDL_Surface *m_surface;
};

#endif
