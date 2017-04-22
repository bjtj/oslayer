#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace OS;
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

		ASSERT(Text::rtrim(Text::toString(1.0f), "0"), ==, "1.");
		ASSERT(Text::rtrim(Text::toString(0.5f), "0"), ==, "0.5");
		ASSERT(Text::rtrim(Text::toString(1.2f), "0"), ==, "1.2");

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

class CommaNumberTest : public TestCase
{
public:
    CommaNumberTest() : TestCase("comma-number-test") {}
    virtual ~CommaNumberTest() {}
	virtual void test() {
		ASSERT(Text::toCommaNumber("0"), ==, "0");
		ASSERT(Text::toCommaNumber("12"), ==, "12");
		ASSERT(Text::toCommaNumber("123"), ==, "123");
		ASSERT(Text::toCommaNumber("1234"), ==, "1,234");
		ASSERT(Text::toCommaNumber("12345"), ==, "12,345");
		ASSERT(Text::toCommaNumber("123456"), ==, "123,456");
		ASSERT(Text::toCommaNumber("1234567"), ==, "1,234,567");
		ASSERT(Text::toCommaNumber("12345678"), ==, "12,345,678");
		ASSERT(Text::toCommaNumber("123456789"), ==, "123,456,789");
		ASSERT(Text::toCommaNumber("1234567890"), ==, "1,234,567,890");
		ASSERT(Text::toCommaNumber("12345678901"), ==, "12,345,678,901");

		ASSERT(Text::toCommaNumber("0."), ==, "0");
		ASSERT(Text::toCommaNumber("12."), ==, "12");
		ASSERT(Text::toCommaNumber("123."), ==, "123");
		ASSERT(Text::toCommaNumber("1234."), ==, "1,234");
		ASSERT(Text::toCommaNumber("12345."), ==, "12,345");
		ASSERT(Text::toCommaNumber("123456."), ==, "123,456");
		ASSERT(Text::toCommaNumber("1234567."), ==, "1,234,567");
		ASSERT(Text::toCommaNumber("12345678."), ==, "12,345,678");
		ASSERT(Text::toCommaNumber("123456789."), ==, "123,456,789");
		ASSERT(Text::toCommaNumber("1234567890."), ==, "1,234,567,890");
		ASSERT(Text::toCommaNumber("12345678901."), ==, "12,345,678,901");

		ASSERT(Text::toCommaNumber("0.0"), ==, "0.0");
		ASSERT(Text::toCommaNumber("0.12"), ==, "0.12");
		ASSERT(Text::toCommaNumber("0.123"), ==, "0.123");
		ASSERT(Text::toCommaNumber("0.1234"), ==, "0.123,4");
		ASSERT(Text::toCommaNumber("0.12345"), ==, "0.123,45");
		ASSERT(Text::toCommaNumber("0.123456"), ==, "0.123,456");
		ASSERT(Text::toCommaNumber("0.1234567"), ==, "0.123,456,7");
		ASSERT(Text::toCommaNumber("0.12345678"), ==, "0.123,456,78");
		ASSERT(Text::toCommaNumber("0.123456789"), ==, "0.123,456,789");



		ASSERT(Text::toCommaNumber("-0"), ==, "-0");
		ASSERT(Text::toCommaNumber("-12"), ==, "-12");
		ASSERT(Text::toCommaNumber("-123"), ==, "-123");
		ASSERT(Text::toCommaNumber("-1234"), ==, "-1,234");
		ASSERT(Text::toCommaNumber("-12345"), ==, "-12,345");
		ASSERT(Text::toCommaNumber("-123456"), ==, "-123,456");
		ASSERT(Text::toCommaNumber("-1234567"), ==, "-1,234,567");
		ASSERT(Text::toCommaNumber("-12345678"), ==, "-12,345,678");
		ASSERT(Text::toCommaNumber("-123456789"), ==, "-123,456,789");
		ASSERT(Text::toCommaNumber("-1234567890"), ==, "-1,234,567,890");
		ASSERT(Text::toCommaNumber("-12345678901"), ==, "-12,345,678,901");

		ASSERT(Text::toCommaNumber("-0."), ==, "-0");
		ASSERT(Text::toCommaNumber("-12."), ==, "-12");
		ASSERT(Text::toCommaNumber("-123."), ==, "-123");
		ASSERT(Text::toCommaNumber("-1234."), ==, "-1,234");
		ASSERT(Text::toCommaNumber("-12345."), ==, "-12,345");
		ASSERT(Text::toCommaNumber("-123456."), ==, "-123,456");
		ASSERT(Text::toCommaNumber("-1234567."), ==, "-1,234,567");
		ASSERT(Text::toCommaNumber("-12345678."), ==, "-12,345,678");
		ASSERT(Text::toCommaNumber("-123456789."), ==, "-123,456,789");
		ASSERT(Text::toCommaNumber("-1234567890."), ==, "-1,234,567,890");
		ASSERT(Text::toCommaNumber("-12345678901."), ==, "-12,345,678,901");

		ASSERT(Text::toCommaNumber("-0.0"), ==, "-0.0");
		ASSERT(Text::toCommaNumber("-0.12"), ==, "-0.12");
		ASSERT(Text::toCommaNumber("-0.123"), ==, "-0.123");
		ASSERT(Text::toCommaNumber("-0.1234"), ==, "-0.123,4");
		ASSERT(Text::toCommaNumber("-0.12345"), ==, "-0.123,45");
		ASSERT(Text::toCommaNumber("-0.123456"), ==, "-0.123,456");
		ASSERT(Text::toCommaNumber("-0.1234567"), ==, "-0.123,456,7");
		ASSERT(Text::toCommaNumber("-0.12345678"), ==, "-0.123,456,78");
		ASSERT(Text::toCommaNumber("-0.123456789"), ==, "-0.123,456,789");

		

		ASSERT(Text::toCommaNumber("+0"), ==, "+0");
		ASSERT(Text::toCommaNumber("+12"), ==, "+12");
		ASSERT(Text::toCommaNumber("+123"), ==, "+123");
		ASSERT(Text::toCommaNumber("+1234"), ==, "+1,234");
		ASSERT(Text::toCommaNumber("+12345"), ==, "+12,345");
		ASSERT(Text::toCommaNumber("+123456"), ==, "+123,456");
		ASSERT(Text::toCommaNumber("+1234567"), ==, "+1,234,567");
		ASSERT(Text::toCommaNumber("+12345678"), ==, "+12,345,678");
		ASSERT(Text::toCommaNumber("+123456789"), ==, "+123,456,789");
		ASSERT(Text::toCommaNumber("+1234567890"), ==, "+1,234,567,890");
		ASSERT(Text::toCommaNumber("+12345678901"), ==, "+12,345,678,901");

		ASSERT(Text::toCommaNumber("+0."), ==, "+0");
		ASSERT(Text::toCommaNumber("+12."), ==, "+12");
		ASSERT(Text::toCommaNumber("+123."), ==, "+123");
		ASSERT(Text::toCommaNumber("+1234."), ==, "+1,234");
		ASSERT(Text::toCommaNumber("+12345."), ==, "+12,345");
		ASSERT(Text::toCommaNumber("+123456."), ==, "+123,456");
		ASSERT(Text::toCommaNumber("+1234567."), ==, "+1,234,567");
		ASSERT(Text::toCommaNumber("+12345678."), ==, "+12,345,678");
		ASSERT(Text::toCommaNumber("+123456789."), ==, "+123,456,789");
		ASSERT(Text::toCommaNumber("+1234567890."), ==, "+1,234,567,890");
		ASSERT(Text::toCommaNumber("+12345678901."), ==, "+12,345,678,901");

		ASSERT(Text::toCommaNumber("+0.0"), ==, "+0.0");
		ASSERT(Text::toCommaNumber("+0.12"), ==, "+0.12");
		ASSERT(Text::toCommaNumber("+0.123"), ==, "+0.123");
		ASSERT(Text::toCommaNumber("+0.1234"), ==, "+0.123,4");
		ASSERT(Text::toCommaNumber("+0.12345"), ==, "+0.123,45");
		ASSERT(Text::toCommaNumber("+0.123456"), ==, "+0.123,456");
		ASSERT(Text::toCommaNumber("+0.1234567"), ==, "+0.123,456,7");
		ASSERT(Text::toCommaNumber("+0.12345678"), ==, "+0.123,456,78");
		ASSERT(Text::toCommaNumber("+0.123456789"), ==, "+0.123,456,789");
	}
};

class FormatTestCase : public TestCase
{
public:
    FormatTestCase() : TestCase("format test case") {
	}
    virtual ~FormatTestCase() {
	}
	virtual void test() {
		ASSERT(Text::format("%s", "hello world"), ==, "hello world");
		ASSERT(Text::nformat(1024, "%s", "hello world"), ==, "hello world");
		ASSERT(Text::nformat(5, "%s", "hello world"), ==, "hell");;
	}
};

class CaseTestCase : public TestCase {
public:
    CaseTestCase() : TestCase("case test") {
	}
    virtual ~CaseTestCase() {
	}
	virtual void test() {
		for (char c = 'a'; c <= 'z'; c++) {
			ASSERT(Text::upcase(c), ==, 'A' + (c - 'a'));
			ASSERT(Text::isUppercase(Text::upcase(c)), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		for (char c = 'A'; c <= 'Z'; c++) {
			ASSERT(Text::downcase(c), ==, 'a' + (c - 'A'));
			ASSERT(Text::isLowercse(Text::downcase(c)), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		for (char c = '0'; c <= '9'; c++) {
			ASSERT(Text::isDigit(c), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		for (char c = '0'; c <= '9'; c++) {
			ASSERT(Text::isHexNumeric(c), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		for (char c = 'a'; c <= 'f'; c++) {
			ASSERT(Text::isHexNumeric(c), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		for (char c = 'A'; c <= 'F'; c++) {
			ASSERT(Text::isHexNumeric(c), ==, true);
			ASSERT(Text::isAlphaNumeric(Text::upcase(c)), ==, true);
		}
		ASSERT(Text::downcase("ABCDEFGHIJKLMNOPQRSTUVWXYZ"), ==, "abcdefghijklmnopqrstuvwxyz");
		ASSERT(Text::upcase("abcdefghijklmnopqrstuvwxyz"), ==, "ABCDEFGHIJKLMNOPQRSTUVWXYZ");
		ASSERT(Text::capitalize("hello world"), ==, "Hello world");
	}
};


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new TextTestCase));
	ts.addTestCase(AutoRef<TestCase>(new SplitTestCase));
	ts.addTestCase(AutoRef<TestCase>(new CommaNumberTest));
	ts.addTestCase(AutoRef<TestCase>(new FormatTestCase));
	ts.addTestCase(AutoRef<TestCase>(new CaseTestCase));

	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}
