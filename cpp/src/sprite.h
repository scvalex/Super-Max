#ifndef SPRITE_H
#define SPRITE_H

#include "issurface.h"
#include "rect.h"

class Sprite {
public:
        Sprite();
        virtual ~Sprite();

        virtual Rect boundingRect() = 0;
        virtual void move(int dx, int dy) = 0;
        virtual void step() = 0;
        virtual void drawOnto(const IsSurface &canvas, int x, int y) = 0;
};

#endif
