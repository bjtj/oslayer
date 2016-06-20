#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Regex.hpp>

using namespace std;
using namespace UTIL;

static bool s_test(const string & regex, const string & text) {
	try {
		Regex r;
		vector<string> tokens = r.tokenize(regex);
		Matcher matcher;
		Iterator<string> iter(tokens);
		r.makeMatcher(iter, matcher);
		MatchResult result = matcher.match(text);
		cout << "matched : " << (result.matched() ? "yes" : "no") << " - " << regex << " / " << text << endl;
		return result.matched();
	} catch (const char * e) {
		cerr << e << endl;
		throw e;
	}
}

class RegexTestCase : public TestCase {
public:
	RegexTestCase() : TestCase("regex") {
	}
	virtual ~RegexTestCase() {
	}

	virtual void test() {
		s_test("hello", "hello");
		s_test("xa*", "x");
		s_test("xa+", "x");
		s_test("xa+", "xa");
		s_test("xa+", "xaa");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "0");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "1");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "1.0");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "0.1");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "0.");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "01");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "-1");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "-1.0");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "-1.1");
		// test("\\-?(([1-9][0-9]*)|0)(\\.[0-9]+)?", "-");
		s_test("hello?", "hello");
		s_test("hello?", "hell");
		s_test("[1-9][0-9]*", "10");
		s_test("[1-9][0-9]*", "1");
		s_test("[1-9][0-9]*", "123");
		s_test("[1-9][0-9]*", "123x");
		s_test("a1?", "a");
		s_test("a1?", "a1");
		s_test("-?0", "-0");
		s_test("-?0", "0");
		s_test("[1-9][0-9]*", "0");
		s_test("[1-9][0-9]*", "1");
		s_test("([1-9][0-9]*|0)", "0");
		s_test("([1-9][0-9]*|0)", "1");
		s_test("-?([1-9][0-9]*|0)", "0");
		s_test("-?([1-9][0-9]*|0)", "1");
		s_test("-?([1-9][0-9]*|0)", "-1");
		s_test("\\.", ".");
		s_test("\\.[0-9]*", ".0");
		s_test("\\.[0-9]*", ".1");
		s_test("\\.[0-9]*", ".1234");
		s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1");
		s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.");
		s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0");
		s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0123");
		s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0123:");
	}
};


/**
 * @brief 
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new RegexTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
