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

bool Event::isQuit() const {
        return (m_event.type == SDL_QUIT);
}

SDL_KeyboardEvent* Event::getKeyDown() {
        if (m_event.type == SDL_KEYDOWN) {
                return &m_event.key;
        }
        return NULL;
}
