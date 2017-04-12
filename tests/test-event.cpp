#include <iostream>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Event.hpp>
#include <liboslayer/Thread.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;


#define LOG(...) cout << "[" << tick_milli() << "]" << __VA_ARGS__ << endl;

/**
 * 
 */
class WorkerThread : public Thread {
private:
	Event & _evt;
	unsigned long _due;
	unsigned long _timeout;
public:
    WorkerThread(Event & evt, unsigned long timeout) : _evt(evt), _due(0), _timeout(timeout) {}
    virtual ~WorkerThread() {}
	virtual void run() {
		LOG("wait...");
		unsigned long _t = tick_milli();
		if (_timeout == 0) {
			_evt.wait();
		} else {
			try {
				_evt.wait(_timeout);
			} catch (TimeoutException e) {
				cerr << "exception: " << e.what() << endl;
			}
		}
		_due = tick_milli() - _t;
		LOG("done...");
	}
	unsigned long & due() {
		return _due;
	}
};

/**
 * 
 */
class EventTestCase : public TestCase {
public:
    EventTestCase() : TestCase("event test") {
	}
    virtual ~EventTestCase() {
	}
	virtual void test() {
		Event e;
		for (int i = 0; i < 2; i++) {
			WorkerThread w(e, i * 2000);
			w.start();
			idle(1000);
			e.notify();
			w.join();
			ASSERT(w.due(), >=, 1000);
		}

		unsigned long _t = tick_milli();
		try {
			e.wait(2000);
			throw "unexpect throw";
		} catch (TimeoutException e) {
			// expect
		}
		ASSERT(tick_milli() - _t, >=, 2000);
	}
};

int main(int argc, char *argv[])
{
    TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new EventTestCase));
	TestReport report(ts.testAll());
	report.validate();
    return 0;
}
