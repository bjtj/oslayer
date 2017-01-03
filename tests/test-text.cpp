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

		printf(" ** to vector\n");
		vec = Text::toVector("a", "A", "b", "B", "c", "C", "d", (const char*)NULL);
		printf(" ** vector to map\n");
		m = Text::toMap(vec);
		ASSERT(m["a"], ==, "A");
		ASSERT(m["b"], ==, "B");
		ASSERT(m["c"], ==, "C");
		ASSERT((m.find("d") != m.end()), ==, true);
		ASSERT(m["d"], ==, "");

		ASSERT(Text::trim(" hello world "), ==, "hello world");
		ASSERT(Text::trim(" hello world"), ==, "hello world");
		ASSERT(Text::trim("hello world "), ==, "hello world");
	}
};

class SplitTestCase : public TestCase {
public:
	SplitTestCase() : TestCase("split") {
	}
	virtual ~SplitTestCase() {
	}
	virtual void test() {
		vector<string> vec = Text::split("a b c", " ");
		ASSERT(vec.size(), ==, 3);
		ASSERT(vec[0], ==, "a");
		ASSERT(vec[1], ==, "b");
		ASSERT(vec[2], ==, "c");

		vec = Text::split("a b  c", " ");
		ASSERT(vec.size(), ==, 3);
		ASSERT(vec[0], ==, "a");
		ASSERT(vec[1], ==, "b");
		ASSERT(vec[2], ==, "c");

		vec = Text::split("a b  c ", " ");
		ASSERT(vec.size(), ==, 3);
		ASSERT(vec[0], ==, "a");
		ASSERT(vec[1], ==, "b");
		ASSERT(vec[2], ==, "c");

		vec = Text::split("a b \tc", " \t");
		ASSERT(vec.size(), ==, 3);
		ASSERT(vec[0], ==, "a");
		ASSERT(vec[1], ==, "b");
		ASSERT(vec[2], ==, "c");

		vec = Text::split("\ta b \tc", " \t");
		ASSERT(vec.size(), ==, 3);
		ASSERT(vec[0], ==, "a");
		ASSERT(vec[1], ==, "b");
		ASSERT(vec[2], ==, "c");
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TextTestCase));
	ts.addTestCase(AutoRef<TestCase>(new SplitTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
