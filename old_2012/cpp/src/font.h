#ifndef FONT_H
#define FONT_H

#include "image.h"
#include <SDL/SDL_ttf.h>
#include <string>

class Font {
public:
        Font(const std::string &filename, int ptSize);
        virtual ~Font();

        Image drawText(const std::string &text);

private:
        TTF_Font *m_font;
};

#endif
