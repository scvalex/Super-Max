#ifndef DEMOLEVEL_H
#define DEMOLEVEL_H

#include "image.h"
#include "issurface.h"
#include "level.h"

class DemoLevel : public Level {
public:
        DemoLevel();
        virtual ~DemoLevel();

        void drawBackground(const IsSurface &canvas);
        void drawStage(const IsSurface &canvas);
        void step();

private:
        Image hills;
        Image hello;

        int x, y;
        int vx, vy;
};

#endif
