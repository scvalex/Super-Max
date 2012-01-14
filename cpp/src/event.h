#ifndef EVENT_H
#define EVENT_H

#include <SDL/SDL.h>

class Event {
private:
        Event();

public:
        virtual ~Event();

        static Event blockForEvent();
        static Event* pollForEvent();

        bool isQuit() const;
        SDL_KeyboardEvent* toKeyDown();

private:
        SDL_Event m_event;
};

#endif
