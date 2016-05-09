#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class LoadLibraryTestCase : public TestCase {
public:
	LoadLibraryTestCase() : TestCase("LoadLibraryTestCase") {}
	virtual ~LoadLibraryTestCase() {}
	virtual void setUp(TestEnvironment & env) {
	}
	virtual void tearDown() {
	}
	virtual void test() {
		Library lib(DATA_PATH, "hello");
		((void (*)(void))lib.getSymbol("hello"))();
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LoadLibraryTestCase));
	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
