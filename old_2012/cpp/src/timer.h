#ifndef TIMER_H
#define TIMER_H

class Timer {
public:
        Timer();
        virtual ~Timer();

        int pause();
        void unpause();

        int ticks();

        bool paused();

        static int globalTicks();
        static void delay(int milli);

private:
        int m_startTicks;
        int m_pausedTicks;

        bool m_paused;
};

#endif
