#include <liboslayer/Random.hpp>
#include <liboslayer/TestSuite.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class RandomTestCase : public TestCase {
public:
	RandomTestCase() : TestCase("RandomTestCase") {}
	virtual ~RandomTestCase() {}

	void test() {
		Random random(time(NULL));
		cout << random.next() << endl;
		cout << random.next() << endl;
		cout << random.next() << endl;
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new RandomTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
