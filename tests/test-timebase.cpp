#include <liboslayer/Timebase.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class Object {
private:
	string msg;
public:
	Object(const string & msg) : msg(msg) {
	}
	virtual ~Object() {
	}
	string sayHello() {
		return msg;
	}
};


class TimebaseTestCase : public TestCase {
public:
	TimebaseTestCase() : TestCase("timebase test") {
	}
	virtual ~TimebaseTestCase() {
	}

	virtual void test() {

		Object obj("hello world");
		Timebase<Object> t(obj, 2000);

		idle(1000);

		ASSERT(t.outdated(), ==, false);
		ASSERT(t.lifetimeRecent(),>=, 1000);
		ASSERT(t.lifetimeFull(), >=, 1000);

		idle(2000);

		ASSERT(t.outdated(), ==, true);
		ASSERT(t.lifetimeRecent(),>=, 3000);
		ASSERT(t.lifetimeFull(), >=, 3000);

		t.prolong();

		ASSERT(t.outdated(), ==, false);
		ASSERT(t.lifetimeRecent(),<=, 10);

		idle(1000);

		ASSERT(t.outdated(), ==, false);
		ASSERT(t.lifetimeRecent(),>=, 1000);
		ASSERT(t.lifetimeFull(), >=, 4000);

	}
};


class TimebaseListTestCase : public TestCase {
public:
	TimebaseListTestCase() : TestCase("timebase list test") {
	}
	virtual ~TimebaseListTestCase() {
	}

	virtual void test() {

		vector<Object> objs;
		objs.push_back(Object("a"));
		objs.push_back(Object("b"));
		objs.push_back(Object("c"));
		objs.push_back(Object("d"));

		TimebaseList<Object> list;
		unsigned long timeout = 500;
		list.add(Timebase<Object>(objs[0], timeout));
		list.add(Timebase<Object>(objs[1], timeout));
		list.add(Timebase<Object>(objs[2], timeout));
		list.add(Timebase<Object>(objs[3], timeout));

		idle(1000);

		for (size_t i = 0; i < list.size(); i++) {
			ASSERT(list[i].outdated(), ==, true);
		}

		list[2].prolong();
		list[3].prolong();

		idle(100);

		ASSERT(list[0].outdated(), ==, true);
		ASSERT(list[1].outdated(), ==, true);
		ASSERT(list[2].outdated(), ==, false);
		ASSERT(list[3].outdated(), ==, false);
	}
};

int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TimebaseTestCase));
	ts.addTestCase(AutoRef<TestCase>(new TimebaseListTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}

