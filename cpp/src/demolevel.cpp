#include "demolevel.h"

#include "event.h"
#include <iostream>

using namespace std;

DemoLevel::DemoLevel() :
        Level(),
        hills("data/hills_at_dawn.png"),
        sans(100, 100)
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
        sans.drawOnto(canvas);
}

void DemoLevel::step() {
        Level::step();
        sans.step();

        char *keys = Event::getKeyState();
        int dx(0), dy(0);
        if (keys[SDLK_UP]) {
                dy = -4;
        }
        if (keys[SDLK_DOWN]) {
                dy = 4;
        }
        if (keys[SDLK_LEFT]) {
                dx = -4;
        }
        if (keys[SDLK_RIGHT]) {
                dx = 4;
        }

        Rect bound = sans.boundingRect();
        if (bound.x() + dx < 0 || width < bound.x() + bound.width() + dx) {
                dx = 0;
        }
        if (bound.y() + dy < 0 ||  height < bound.y() + bound.height() + dy) {
                dy = 0;
        }

        sans.move(dx, dy);
}
