#ifndef SCREEN_H
#define SCREEN_H

#include "issurface.h"
#include <string>

class SDL_Surface;

class Screen : public IsSurface {
public:
        Screen(int width, int height);
        virtual ~Screen();

        static Screen& screen();

        void setTitle(const std::string &title);

        void flip();

        SDL_Surface* sdlSurface() const;

private:
        SDL_Surface *m_surface;

        static Screen* instance;
};

#endif
