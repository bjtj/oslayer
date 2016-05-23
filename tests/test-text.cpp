#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace UTIL;

class TextTestCase : public TestCase {
public:
	TextTestCase() : TestCase("TextTestCase") {}
	virtual ~TextTestCase() {}

	virtual void test() {
		ASSERT(Text::toFloat("1.0"), ==, 1.0f);
		ASSERT(Text::toFloat("0.5"), ==, 0.5f);
		ASSERT(Text::toFloat("1.2"), ==, 1.2f);
		
		ASSERT(Text::toString(1.0f).substr(0, 3), ==, "1.0");
		ASSERT(Text::toString(0.5f).substr(0, 3), ==, "0.5");
		ASSERT(Text::toString(1.2f).substr(0, 3), ==, "1.2");

		vector<string> vec;
		vec.push_back("a");
		vec.push_back("A");
		vec.push_back("b");
		vec.push_back("B");
		vec.push_back("c");
		map<string, string> m = Text::toMap(vec);
		ASSERT(m["a"], ==, "A");
		ASSERT(m["b"], ==, "B");
		ASSERT((m.find("c") != m.end()), ==, true);
		ASSERT(m["c"], ==, "");

		vec = Text::toVector("a", "A", "b", "B", "c", "C", "d", NULL);
		m = Text::toMap(vec);
		ASSERT(m["a"], ==, "A");
		ASSERT(m["b"], ==, "B");
		ASSERT(m["c"], ==, "C");
		ASSERT((m.find("d") != m.end()), ==, true);
		ASSERT(m["d"], ==, "");
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TextTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
