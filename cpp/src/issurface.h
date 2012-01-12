#ifndef ISSURFACE_H
#define ISSURFACE_H

#include <SDL/SDL.h>

class IsSurface {
public:
        virtual SDL_Surface* getSurface() const = 0;
};

#endif
