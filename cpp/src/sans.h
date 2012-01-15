#ifndef SANS_H
#define SANS_H

#include "sprite.h"

class Image;

class Sans : public Sprite {
public:
        Sans(int x, int y);
        ~Sans();

        Rect boundingRect();
        void move(int dx, int dy);
        void step();
        void drawOnto(const IsSurface &canvas);

private:
        enum State {
                STANDING_STILL = 0,
                WALKING_1 = 1,
                WALKING_2 = 2,
                WALKING_3 = 3
        };

        State NEXT_STATE[4];

        enum Facing {
                FACING_RIGHT = 0,
                FACING_LEFT = 1
        };

        int m_x, m_y;
        State state;
        Facing facing;

        Image* imgs[8];

        Image* currentImage();
};

#endif
