#include "demolevel.h"

DemoLevel::DemoLevel() :
        Level(),
        hills("data/hills_at_dawn.png")
{
}

DemoLevel::~DemoLevel() {
}

void DemoLevel::drawBackground(const IsSurface &canvas) {
        Level::drawBackground(canvas);
        for (int i(0); i < 2; ++i) {
                hills.drawOnto(canvas, i * hills.width(), 0);
        }
}
