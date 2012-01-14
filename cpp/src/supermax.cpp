#include <chipmunk/chipmunk.h>
#include <cstdio>
#include "event.h"
#include "font.h"
#include "graphics.h"
#include "image.h"
#include <iostream>
#include "screen.h"
#include <SDL/SDL.h>
#include <stdexcept>
#include <string>
#include <sstream>
#include "timer.h"

using namespace std;

const int FRAMES_PER_SECOND = 30;
const int SCREEN_WIDTH = 640;
const int SCREEN_HEIGHT = 480;

void drawTiledBackground(Image &img, int count);
void showFrameRate(int frame);
void showPaused(bool paused);

int main(int argc, char *argv[]) {
        cout << "Starting Super Max" << endl;

        Graphics graphics;

        Screen screen(SCREEN_WIDTH, SCREEN_HEIGHT);
        screen.setTitle("Super Max");

        Image hills("data/hills_at_dawn.png");
        bool quit(false);
        bool paused(false), pausable(true);

        for (int frame(0); !quit; (!paused) ? ++frame : frame) {
                Timer fps;

                drawTiledBackground(hills, 2);
                showFrameRate(frame);
                showPaused(paused);
                screen.flip();

                char *keys = Event::getKeyState();
                if (keys[SDLK_UP]) {
                }
                if (keys[SDLK_DOWN]) {
                }
                if (keys[SDLK_LEFT]) {
                }
                if (keys[SDLK_RIGHT]) {
                }
                if (keys[SDLK_p]) {
                        if (pausable) {
                                paused = !paused;
                                pausable = false;
                        }
                } else {
                        pausable = true;
                }

                while (Event *e = Event::pollForEvent()) {
                        if (e->isQuit()) {
                                cout << "Quitting" << endl;
                                quit = true;
                        }
                }

                if (fps.ticks() < 1000 / FRAMES_PER_SECOND) {
                        Timer::delay((1000 / FRAMES_PER_SECOND) -
                                     fps.ticks());
                }
        }

        // cpVect is a 2D vector and cpv() is a shortcut for
        // initializing them.
        cpVect gravity = cpv(0, -100);

        // Create an empty space.
        cpSpace *space = cpSpaceNew();
        cpSpaceSetGravity(space, gravity);

        // Add a static line segment shape for the ground.  We'll make
        // it slightly tilted so the ball will roll off.  We attach it
        // to space->staticBody to tell Chipmunk it shouldn't be
        // movable.
        cpShape *ground = cpSegmentShapeNew(space->staticBody, cpv(-20, 5),
                                            cpv(20, -5), 0);
        cpShapeSetFriction(ground, 1);
        cpSpaceAddShape(space, ground);

        // Now let's make a ball that falls onto the line and rolls
        // off.  First we need to make a cpBody to hold the physical
        // properties of the object.  These include the mass,
        // position, velocity, angle, etc. of the object.  Then we
        // attach collision shapes to the cpBody to give it a size and
        // shape.

        cpFloat radius = 5;
        cpFloat mass = 1;

        // The moment of inertia is like mass for rotation
        // Use the cpMomentFor*() functions to help you approximate it.
        cpFloat moment = cpMomentForCircle(mass, 0, radius, cpvzero);

        // The cpSpaceAdd*() functions return the thing that you are adding.
        // It's convenient to create and add an object in one line.
        cpBody *ballBody = cpSpaceAddBody(space, cpBodyNew(mass, moment));
        cpBodySetPos(ballBody, cpv(0, 15));

        // Now we create the collision shape for the ball.  You can
        // create multiple collision shapes that point to the same
        // body.  They will all be attached to the body and move
        // around to follow it.
        cpShape *ballShape = cpSpaceAddShape(space,
                                             cpCircleShapeNew(ballBody,
                                                              radius,
                                                              cpvzero));
        cpShapeSetFriction(ballShape, 0.7);

        // Now that it's all set up, we simulate all the objects in
        // the space by stepping forward through time in small
        // increments called steps.  It is *highly* recommended to use
        // a fixed size time step.
        cpFloat timeStep = 1.0/60.0;
        for (cpFloat time = 0; time < 2; time += timeStep) {
                cpVect pos = cpBodyGetPos(ballBody);
                cpVect vel = cpBodyGetVel(ballBody);
                printf("Time is %5.2f. ballBody is at "
                       "(%5.2f, %5.2f). It's velocity "
                       "is (%5.2f, %5.2f)\n",
                       time, pos.x, pos.y, vel.x, vel.y);

                cpSpaceStep(space, timeStep);
        }

        // Clean up our objects and exit!
        cpShapeFree(ballShape);
        cpBodyFree(ballBody);
        cpShapeFree(ground);
        cpSpaceFree(space);

        return 0;
}

void drawTiledBackground(Image &img, int count) {
        for (int i(0); i < count; ++i) {
                img.drawOnto(Screen::screen(), i * img.width(), 0);
        }
}

void showFrameRate(int frame) {
        stringstream text;
        text << frame;

        Image msgImg(Font("data/libertine.ttf", 16)
                     .drawText(text.str()));
        msgImg.drawOnto(Screen::screen(),
                        SCREEN_WIDTH - msgImg.width() - 20, 20);
}

void showPaused(bool paused) {
        if (paused) {
                Image msgImg(Font("data/libertine.ttf", 32)
                             .drawText("Paused"));
                msgImg.drawOnto(Screen::screen(),
                                (SCREEN_WIDTH - msgImg.width()) / 2,
                                (SCREEN_HEIGHT - msgImg.height()) / 2);
        }
}
