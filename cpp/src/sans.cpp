#include "sans.h"

#include <iostream>
#include <sstream>
#include "image.h"

using namespace std;

Sans::Sans(int x, int y) :
        m_x(x), m_y(y),
        state(STANDING_STILL),
        facing(FACING_RIGHT)
{
        for (int i(0); i < 8; ++i) {
                stringstream fname;
                fname << "data/sans/" << (i+1) << ".png";
                imgs[i] = new Image(fname.str());
        }

        NEXT_STATE[STANDING_STILL] = STANDING_STILL;
        NEXT_STATE[WALKING_1] = WALKING_2;
        NEXT_STATE[WALKING_2] = WALKING_3;
        NEXT_STATE[WALKING_3] = WALKING_1;
}

Sans::~Sans() {
        for (int i(0); i < 8; ++i) {
                delete imgs[i];
        }
}

Rect Sans::boundingRect() {
        Image* img = currentImage();
        return Rect(m_x - (img->width() / 2), m_y - (img->height() / 2),
                    img->width(), img->height());
}

void Sans::move(int dx, int dy) {
        m_x += dx;
        m_y += dy;

        if (dx == 0) {
                state = STANDING_STILL;
        } else {
                if (state == STANDING_STILL) {
                        state = WALKING_1;
                }
                if (dx > 0) {
                        facing = FACING_RIGHT;
                } else if (dx < 0) {
                        facing = FACING_LEFT;
                }
        }
}

void Sans::step() {
        static int i(0);
        if (++i % 4 == 0) {
                state = NEXT_STATE[state];
        }
}

void Sans::drawOnto(const IsSurface &canvas) {
        Rect bound = boundingRect();
        currentImage()->drawOnto(canvas, bound.x(), bound.y());
}

Image* Sans::currentImage() {
        return imgs[facing * 4 + state];
}

