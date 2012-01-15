#include "rect.h"

Rect::Rect(int x, int y, int w, int h) :
        m_x(x), m_y(y),
        m_w(w), m_h(h)
{
        m_rect.x = m_x;
        m_rect.y = m_y;
        m_rect.w = m_w;
        m_rect.h = m_h;
}

Rect::~Rect() {
}

int Rect::x() const {
        return m_x;
}

int Rect::y() const {
        return m_y;
}

int Rect::width() const {
        return m_w;
}

int Rect::height() const {
        return m_h;
}

SDL_Rect* Rect::sdlRect() {
        return &m_rect;
}
