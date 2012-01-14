#ifndef LEVEL_H
#define LEVEL_H

#include "issurface.h"

class Level {
public:
        Level();
        virtual ~Level();

        virtual void drawParallax(const IsSurface &canvas);
        virtual void drawBackground(const IsSurface &canvas);
        virtual void drawStage(const IsSurface &canvas);
        virtual void drawForeground(const IsSurface &canvas);
        virtual void drawAll(const IsSurface &canvas);
};

#endif
