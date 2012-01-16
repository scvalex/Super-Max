#include "issurface.h"

#include <iostream>
#include <stdexcept>
#include <string>

using namespace std;

SDL_Rect* IsSurface::clip() const {
        return NULL;
}

void IsSurface::drawOnto(const IsSurface &canvas, int x, int y) {
        SDL_Rect offset;
        offset.x = x;
        offset.y = y;

        drawInternal(surface(), canvas.surface(), clip(), &offset);
}

int IsSurface::width() const {
        return surface()->w;
}

int IsSurface::height() const {
        return surface()->h;
}

void IsSurface::drawInternal(SDL_Surface *img, SDL_Surface *canvas,
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
