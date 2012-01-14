#include "demolevel.h"

#include "event.h"
#include <iostream>

using namespace std;

DemoLevel::DemoLevel() :
        Level(),
        hills("data/hills_at_dawn.png"),
        hello("data/hello.png"),
        x(20), y(20)
{
}

DemoLevel::~DemoLevel() {
}

void DemoLevel::drawBackground(const IsSurface &canvas) {
        Level::drawBackground(canvas);
        for (int i(0); i < ((width + 1) / hills.width()); ++i) {
                hills.drawOnto(canvas, i * hills.width(), 0);
        }
}

void DemoLevel::drawStage(const IsSurface &canvas) {
        Level::drawStage(canvas);
        hello.drawOnto(canvas, x, y);
}

void DemoLevel::step() {
        Level::step();

        char *keys = Event::getKeyState();
        int nx(x), ny(y);
        if (keys[SDLK_UP]) {
                ny -= 4;
        }
        if (keys[SDLK_DOWN]) {
                ny += 4;
        }
        if (keys[SDLK_LEFT]) {
                nx -= 4;
        }
        if (keys[SDLK_RIGHT]) {
                nx += 4;
        }

        if (0 <= nx && nx < width - hello.width()) {
                x = nx;
        }
        if (0 <= ny && ny < height - hello.height()) {
                y = ny;
        }
}
