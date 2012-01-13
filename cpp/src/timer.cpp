#include "timer.h"

#include <SDL/SDL.h>

int Timer::getTicks() {
        return SDL_GetTicks();
}
