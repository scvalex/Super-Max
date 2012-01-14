#include "event.h"

#include <stdexcept>

using namespace std;

Event::Event() {
}

Event::~Event() {
}

Event Event::blockForEvent() {
        Event e;
        if (!SDL_WaitEvent(&e.m_event)) {
                throw runtime_error("Error blocking for event.");
        }
        return e;
}

Event* Event::pollForEvent() {
        Event *e = new Event;
        if (!SDL_PollEvent(&e->m_event)) {
                delete e;
                return NULL;
        }
        return e;
}

char* Event::getKeyState() {
        SDL_PumpEvents();
        return (char*)SDL_GetKeyState(NULL);
}

bool Event::isQuit() const {
        return (m_event.type == SDL_QUIT);
}
