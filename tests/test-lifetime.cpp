#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Lifetime.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class LifetimeTestCase : public TestCase {
private:
	class MyObject : public Lifetime {
	public:
		MyObject() {/**/}
		virtual ~MyObject() {/**/}
	};

public:
	LifetimeTestCase() : TestCase("heap") {
	}
	virtual ~LifetimeTestCase() {
	}

	virtual void test() {
		MyObject obj;

		idle(500);

		ASSERT(obj.lifetime(), >=, 500);
		obj.resetLifetime();
		ASSERT(obj.lifetime(), <=, 10);

		idle(500);

		ASSERT(obj.lifetime(), >=, 500);
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LifetimeTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
