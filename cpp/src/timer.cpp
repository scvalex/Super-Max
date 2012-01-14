#include "timer.h"

#include <cassert>
#include <SDL/SDL.h>

Timer::Timer() :
        m_startTicks(SDL_GetTicks()),
        m_pausedTicks(0),
        m_paused(false)
{
}

Timer::~Timer() {
}

int Timer::pause() {
        assert(!m_paused);

        m_paused = true;
        m_pausedTicks = SDL_GetTicks() - m_startTicks;
        return m_pausedTicks;
}

void Timer::unpause() {
        assert(m_paused);

        m_paused = false;
        m_startTicks = SDL_GetTicks() - m_pausedTicks;
        m_pausedTicks = 0;
}

int Timer::ticks() {
        if (m_paused) {
                return m_pausedTicks;
        } else {
                return SDL_GetTicks() - m_startTicks;
        }
}

bool Timer::paused() {
        return m_paused;
}

int Timer::globalTicks() {
        return SDL_GetTicks();
}

void Timer::delay(int milli) {
        SDL_Delay(milli);
}
