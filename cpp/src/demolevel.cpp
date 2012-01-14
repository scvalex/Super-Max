#include "demolevel.h"

#include "event.h"
#include <iostream>

using namespace std;

DemoLevel::DemoLevel() :
        Level(),
        hills("data/hills_at_dawn.png"),
        hello("data/hello.png"),
        x(20), y(20),
        vx(0), vy(0)
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

void DemoLevel::drawStage(const IsSurface &canvas) {
        Level::drawStage(canvas);
        hello.drawOnto(canvas, x, y);
}

void DemoLevel::step() {
        Level::step();

        char *keys = Event::getKeyState();
        if (keys[SDLK_UP]) {
                vy -= 1;
        }
        if (keys[SDLK_DOWN]) {
                vy += 1;
        }
        if (keys[SDLK_LEFT]) {
                vx -= 1;
        }
        if (keys[SDLK_RIGHT]) {
                vx += 1;
        }

        cout << "V: " << vx << ", " << vy << endl;

        x += vx;
        y += vy;
}
