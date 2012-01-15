#ifndef ISSURFACE_H
#define ISSURFACE_H

#include <SDL/SDL.h>

class IsSurface {
public:
        virtual SDL_Surface* surface() const = 0;
        virtual SDL_Rect* clip() const;
};

#endif
