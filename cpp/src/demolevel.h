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

private:
        Image hills;
};

#endif
