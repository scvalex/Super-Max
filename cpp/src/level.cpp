#include "level.h"

Level::Level() {
}

Level::~Level() {
}

void Level::drawParallax(const IsSurface &canvas) {
}

void Level::drawBackground(const IsSurface &canvas) {
}

void Level::drawStage(const IsSurface &canvas) {
}

void Level::drawForeground(const IsSurface &canvas) {
}

void Level::drawAll(const IsSurface &canvas) {
        drawParallax(canvas);
        drawBackground(canvas);
        drawStage(canvas);
        drawForeground(canvas);
}
