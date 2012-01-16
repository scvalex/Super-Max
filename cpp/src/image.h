#ifndef IMAGE_H
#define IMAGE_H

#include "issurface.h"
#include <string>

class SDL_Surface;
class SDL_Rect;

class Image : public IsSurface {
public:
        Image(const std::string &filename);
        Image(SDL_Surface *surface);
        virtual ~Image();

        int width() const;
        int height() const;

        SDL_Surface* surface() const;

private:
        SDL_Surface *m_surface;
};

#endif
