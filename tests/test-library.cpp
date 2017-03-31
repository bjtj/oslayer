#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Library.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class LoadLibraryTestCase : public TestCase {
public:
	LoadLibraryTestCase() : TestCase("LoadLibraryTestCase") { /**/ }
	virtual ~LoadLibraryTestCase() { /**/ }
	virtual void setUp(TestEnvironment & env) {
	}
	virtual void tearDown() {
	}
	virtual void test() {
		try {
			Library lib(DATA_PATH, "hello");
			((void (*)(void))*lib.getSymbol("hello"))();
		} catch (Exception e) {
			cerr << e.toString() << endl;
		}
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new LoadLibraryTestCase));
	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
