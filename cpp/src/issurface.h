#ifndef ISSURFACE_H
#define ISSURFACE_H

#include <SDL/SDL.h>

class IsSurface {
public:
        virtual SDL_Surface* sdlSurface() const = 0;
        virtual SDL_Rect* clip() const;
        virtual int width() const;
        virtual int height() const;

        virtual void drawOnto(const IsSurface &canvas, int x = 0, int y = 0);

private:
        void drawInternal(SDL_Surface *img, SDL_Surface *canvas,
                          SDL_Rect *clip, SDL_Rect *offset);
};

#endif
