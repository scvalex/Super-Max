#ifndef DEMOLEVEL_H
#define DEMOLEVEL_H

#include "image.h"
#include "issurface.h"
#include "level.h"

class DemoLevel : public Level {
public:
        DemoLevel();
        virtual ~DemoLevel();

        static const int width = 640;
        static const int height = 480;

        void drawBackground(const IsSurface &canvas);
        void drawStage(const IsSurface &canvas);
        void step();

private:
        Image hills;
        Image hello;

        int x, y;
};

#endif
