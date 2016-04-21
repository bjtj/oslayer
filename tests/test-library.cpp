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
		// load library - assess
		// retreive the symbols - asesses
		// close library - assess
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LoadLibraryTestCase));
	
	TestReporter report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
