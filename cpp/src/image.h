#ifndef IMAGE_H
#define IMAGE_H

#include "issurface.h"
#include <string>

class SDL_Surface;

class Image : public IsSurface {
public:
        Image(const std::string &filename);
        virtual ~Image();

        void drawOnto(const IsSurface &canvas, int x = 0, int y = 0);

        SDL_Surface* getSurface() const;

private:
        SDL_Surface *m_surface;
};

#endif
