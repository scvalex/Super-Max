#include "demolevel.h"

#include <cmath>
#include "event.h"
#include <iostream>

using namespace std;

DemoLevel::DemoLevel() :
        Level(),
        background(width, height, true),
        parralax(width, height),
        sans(100, 100),
        sceneX(0), sceneY(0),
        sceneW(-1), sceneH(-1)
{
        Image hills("data/hills_at_dawn.png");
        for (int i(0); i < ceil(1.0 * width / hills.width()); ++i) {
                hills.drawOnto(parralax, i * hills.width(), 0);
        }

        Image hillsBg("data/hills_at_dawn_bg.png");
        for (int i(0); i < ceil(1.0 * width / hillsBg.width()); ++i) {
                hillsBg.drawOnto(background, i * hillsBg.width(), 0);
        }
}

DemoLevel::~DemoLevel() {
}

void DemoLevel::drawParallax(const IsSurface &canvas) {
        Level::drawParallax(canvas);

        sceneW = canvas.width();
        sceneH = canvas.height();

        parralax.clip(sceneX / 2, sceneY, sceneW, sceneH)
                .drawOnto(canvas, 0, 0);
}

void DemoLevel::drawBackground(const IsSurface &canvas) {
        Level::drawBackground(canvas);

        sceneW = canvas.width();
        sceneH = canvas.height();

        background.clip(sceneX, sceneY, sceneW, sceneH)
                  .drawOnto(canvas, 0, 0);
}

void DemoLevel::drawStage(const IsSurface &canvas) {
        Level::drawStage(canvas);
        sans.drawOnto(canvas, sans.x() - sceneX, sans.y() - sceneY);
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
        if (bound.y() + dy < 0 || height < bound.y() + bound.height() + dy) {
                dy = 0;
        }

        const int screenMargin = 30;
        if (dx > 0 && bound.x() - sceneX + dx >= sceneW - screenMargin - bound.width()) {
                if (sceneX + sceneW + dx < width) {
                        sceneX += dx;
                }
        } else if (dx < 0 && bound.x() - sceneX + dx <= screenMargin) {
                if (sceneX + dx >= 0) {
                        sceneX += dx;
                }
        }

        sans.move(dx, dy);
}
