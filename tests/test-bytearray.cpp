#include <liboslayer/ByteArray.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace UTIL;

/**
 * byte array test case
 */
class ByteArrayTestCase : public TestCase {
public:
    ByteArrayTestCase() : TestCase("byte-array-test-case") {
	}
    virtual ~ByteArrayTestCase() {
	}
	virtual void test() {
	}
};

/**
 * main
 */
int main(int argc, char *argv[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new ByteArrayTestCase));;
	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
