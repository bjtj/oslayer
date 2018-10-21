#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Timer.hpp>
#include "utils.hpp"

using namespace std;
using namespace osl;


class MyTask : public TimerTask {
private:
    TimePin & _pin;
public:
    MyTask(TimePin & pin) : _pin(pin) {}
    virtual ~MyTask() {}

    virtual void onTask() {
	cout << "[" << _pin.elapsed() << "] do it!" << endl;
	ASSERT(_pin.elapsed(), >=, 1000);
    }
};

class TimerThread : public Thread {
private:
    TimePin & _pin;
    TimerLooper looper;
public:
    TimerThread(TimePin & pin) : _pin(pin) {}
    virtual ~TimerThread() {}

    virtual void run() {
	looper.interval(1000, AutoRef<TimerTask>(new MyTask(_pin)));
	looper.loop();
    }

    void br() {
	looper.stop();
    }
};


static void test_timer() {

    TimePin pin;

    TimerThread tt(pin);
    tt.start();

    cout << pin.elapsed() << endl;

    idle(2 * 1000);

    cout << pin.elapsed() << endl;
    ASSERT(pin.elapsed(), >=, 2000 - 10);

    tt.br();
    tt.join();
}

int main(int argc, char *args[]) {

    test_timer();
    
    return 0;
}

