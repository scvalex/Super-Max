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

        void drawOnto(const IsSurface &canvas, int x = 0, int y = 0);
        void drawOntoClip(const IsSurface &canvas, int dx, int dy,
                          int sx, int sy, int sw, int sh);

        SDL_Surface* surface() const;

private:
        void drawInternal(SDL_Surface *img, SDL_Surface *canvas,
                          SDL_Rect *clip, SDL_Rect *offset);

        SDL_Surface *m_surface;
};

#endif
