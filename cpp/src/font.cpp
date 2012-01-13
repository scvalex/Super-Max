#include "font.h"

#include "assert.h"
#include <SDL/SDL.h>
#include <stdexcept>

using namespace std;

Font::Font(const string &filename, int ptSize) {
        m_font = TTF_OpenFont(filename.c_str(), ptSize);
        if (!m_font) {
                throw runtime_error("Could not load font " + filename
                                    + ": " + TTF_GetError());
        }
}

Font::~Font() {
        TTF_CloseFont(m_font);
}

Image Font::drawText(const string &text) {
        assert(text.size() > 0);

        SDL_Color textColor = {255, 255, 255};
        SDL_Surface *surface = TTF_RenderText_Solid(m_font, text.c_str(),
                                                    textColor);
        if (!surface) {
                throw runtime_error(string("Could not draw text: ") +
                                    TTF_GetError());
        }

        return Image(surface);
}
