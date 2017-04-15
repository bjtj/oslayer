#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Observer.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

static int s_value = 0;
static int s_monitored_value = 0;

class Subject : public Observable {
public:
	Subject() {}
	virtual ~Subject() {}

	void setValue(int v) {
		s_value = v;
		notifyObservers();
	}

	int getValue() {
		return s_value;
	}
};

class ValueMonitor : public Observer {
public:
	ValueMonitor() {}
	virtual ~ValueMonitor() {}
	virtual void onUpdate(Observable * target) {
		Subject * sub = (Subject*)target;
		s_monitored_value = sub->getValue();
		ASSERT(sub->getValue(), ==, s_value);
	}
};


class ObserverTestCase : public TestCase {
public:
	ObserverTestCase() : TestCase("Obserer test") {}
	virtual ~ObserverTestCase() {}

	virtual void test() {
		Subject sub;
		ValueMonitor vm;
		sub.addObserver(&vm);

		sub.setValue(0);
		sub.setValue(1);
		sub.setValue(2);
		sub.setValue(3);

		ASSERT(s_monitored_value, ==, s_value);
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ObserverTestCase));
	TestReport report(ts.testAll());
	cout << report.toString() << endl;
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
