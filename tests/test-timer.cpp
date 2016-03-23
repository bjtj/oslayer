#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/Timer.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace UTIL;

TimePin pin;

class MyTask : public TimerTask {
private:
public:
    MyTask() {}
    virtual ~MyTask() {}
	virtual void doTask() {
		cout << "[" << pin.elapsed() << "] do it!" << endl;
		ASSERT(pin.elapsed(), >=, 1000);
		ASSERT(pin.elapsed(), <=, 1010);
	}
};

class TimerThread : public Thread {
private:
	TimerLooper looper;
public:
    TimerThread() {}
    virtual ~TimerThread() {}

	virtual void run() {
		TimerSchedule schedule(0, 1000, 0);
		AutoRef<TimerTask> task(new MyTask);
		TimerSession session(schedule, task);
		
		looper.addSession(session);
		looper.loop();
	}

	void br() {
		looper.stop();
	}
};


static void test_timer() {

	TimerThread tt;
	tt.start();

	cout << pin.elapsed() << endl;

	idle(2 * 1000);

	cout << pin.elapsed() << endl;

	tt.br();
}

int main(int argc, char *args[]) {

	test_timer();
    
    return 0;
}
