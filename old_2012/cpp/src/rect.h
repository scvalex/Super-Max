#ifndef RECT_H
#define RECT_H

#include <SDL/SDL.h>

class Rect {
public:
        Rect(int x, int y, int w, int h);
        virtual ~Rect();

        int x() const;
        int y() const;
        int width() const;
        int height() const;
        SDL_Rect* sdlRect();

private:
        int m_x, m_y;
        int m_w, m_h;
        SDL_Rect m_rect;
};

#endif
