#ifndef DEMOLEVEL_H
#define DEMOLEVEL_H

#include "image.h"
#include "issurface.h"
#include "level.h"
#include "sans.h"
#include "surface.h"

class DemoLevel : public Level {
public:
        DemoLevel();
        virtual ~DemoLevel();

        static const int width = 2000;
        static const int height = 480;

        void drawParallax(const IsSurface &canvas);
        void drawBackground(const IsSurface &canvas);
        void drawStage(const IsSurface &canvas);
        void step();

private:
        Surface background;
        Surface parralax;
        Sans sans;

        int sceneX, sceneY;

        int sceneW, sceneH;
};

#endif
