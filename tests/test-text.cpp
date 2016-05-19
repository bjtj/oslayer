#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>

using namespace UTIL;

class TextTestCase : public TestCase {
public:
	TextTestCase() : TestCase("TextTestCase") {}
	virtual ~TextTestCase() {}

	virtual void test() {
		ASSERT(Text::toFloat("1.0"), ==, 1.0f);
		ASSERT(Text::toFloat("0.5"), ==, 0.5f);
		ASSERT(Text::toFloat("1.2"), ==, 1.2f);
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TextTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
