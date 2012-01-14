#ifndef TIMER_H
#define TIMER_H

class Timer {
public:
        Timer();
        virtual ~Timer();

        int pause();
        void unpause();

        int getTicks();

        bool paused();

        static int getGlobalTicks();

private:
        int m_startTicks;
        int m_pausedTicks;

        bool m_paused;
};

#endif
