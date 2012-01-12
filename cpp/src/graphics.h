#ifndef GRAPHICS_H
#define GRAPHICS_H

class Graphics {
public:
        /** Do NOT do anything until Graphics is instantiated. */
        Graphics();
        virtual ~Graphics();

        void delay(int milli);

private:
        static bool initialised;
};

#endif
