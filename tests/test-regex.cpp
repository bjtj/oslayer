#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Regex.hpp>

using namespace std;
using namespace osl;


/**
 * regex test and report
 */
static MatchType s_test(const string & regex, const string & text) {
    try {
	Regex::debug() = true;
	Matcher::debug() = true;
	Regex r(regex);
	// AutoRef<Matcher> matcher = r.makeMatcher();
	// MatchResult result = matcher->match(text);
	MatchResult result = r.match(text);
	cout << " # matched : " << result.matchType().toString() << " - " << regex << " / " << text << endl;
	vector<string> groups = result.groups();
	if (groups.size() > 0) {
	    cout << "[GROUPS]" << endl;
	    for (vector<string>::iterator iter = groups.begin(); iter != groups.end(); iter++) {
		cout << " - " << *iter << endl;
	    }
	}
		
	return result.matchType();
    } catch (const char * e) {
	cerr << " ERR: " << e << endl;
	throw e;
    }
}

/**
 * regex internal test case
 */
class RegexInternalTestCase : public TestCase {
private:
public:
    RegexInternalTestCase() : TestCase("regex-internal") {
    }
    virtual ~RegexInternalTestCase() {
    }
    virtual void test() {
	Regex r;
	vector<string> tokens = r.tokenize("(.+)@");
	Iterator<string> iter(tokens);
	AutoRef<Matcher> matcher = r.makeMatcher(iter);
	cout << "Root Matcher : " << matcher->toString() << endl;
	AutoRef<Matcher> next = matcher->nextMatcher(&(matcher->elements()[0]));
	ASSERT(next.nil(), ==, false);
	cout << "Next Matcher: " << next->toString() << endl;

	cout << "2-Matcher: " << matcher->elements()[0]->elements()[0]->toString() << endl;
	cout << "2-Matcher parent: " << matcher->elements()[0]->elements()[0]->getParent()->toString() << endl;
	cout << "2-Matcher 2-parent: " << matcher->elements()[0]->elements()[0]->getParent()->getParent()->toString() << endl;

	next = matcher->elements()[0]->nextMatcher(&(matcher->elements()[0]->elements()[0]));
	ASSERT(next.nil(), ==, false);
	cout << "2-Next Matcher: " << next->toString() << endl;
    }
};

/**
 * regex test case
 */
class RegexTestCase : public TestCase {
public:
    RegexTestCase() : TestCase("regex") {
    }
    virtual ~RegexTestCase() {
    }

    virtual void test() {
	ASSERT(s_test("hello", "hello"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("xa*", "x"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("xa+", "x"), ==, MatchType::NOT_MATCHED);
	ASSERT(s_test("xa+", "xa"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("xa+", "xaa"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("hello?", "hello"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("hello?", "hell"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "10"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "123"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "123x"), ==, MatchType::PARTIAL_MATCHED);
	ASSERT(s_test("a1?", "a"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("a1?", "a1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?0", "-0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?0", "0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "0"), ==, MatchType::NOT_MATCHED);
	ASSERT(s_test("[1-9][0-9]*", "1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("([1-9][0-9]*|0)", "0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("([1-9][0-9]*|0)", "1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)", "0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)", "1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)", "-1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("\\.", "."), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("\\.[0-9]*", ".0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("\\.[0-9]*", ".1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("\\.[0-9]*", ".1234"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1."), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0123"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("-?([1-9][0-9]*|0)(\\.[0-9]*)?", "-1.0123:"), ==, MatchType::PARTIAL_MATCHED);
	ASSERT(s_test(".+", "test@example.com"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test(".+@", "test@"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test(".+@(.+\\.com)", "test@example.com"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("(.+)@(.+\\.com)", "test@example.com"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[^b][^a][^0]", "abc"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[a-cA-C]+", "abcABC"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("[a-cA-C]+", "abcABCD"), ==, MatchType::PARTIAL_MATCHED);
	ASSERT(s_test(".^.", "a^b"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test(".^.", "b^a"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("^^HELLO", "^HELLO"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test(".$.", "a$b"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test(".$.", "b$a"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("y..h", "yeah"), ==, MatchType::FULL_MATCHED);
	ASSERT(s_test("HELLO$$", "HELLO$"), ==, MatchType::FULL_MATCHED);
    }
};


class RegexSearchTest : public TestCase
{
public:
    RegexSearchTest() : TestCase("regex-search") {
    }
    virtual ~RegexSearchTest() {
    }

    void test() {

	Regex::debug() = false;
	Matcher::debug() = false;
		
	string text = "say hello yeah!";
	Range range = Regex("hello").search(text);
	ASSERT(range.start(), ==, 4);
	ASSERT(range.matchResult().length(), ==, 5);
		
	range = Regex("yeah").search(text);
	ASSERT(range.start(), ==, 10);
	ASSERT(range.matchResult().length(), ==, 4);

	range = Regex("y..h").search(text);
	ASSERT(range.start(), ==, 10);
	ASSERT(range.matchResult().length(), ==, 4);

	range = Regex("(hello yeah)").search(text);
	ASSERT(range.start(), ==, 4);
	ASSERT(range.matchResult().length(), ==, 10);

	text = "http://youtube.googleapis.com/v/4e_kz79tjb8?version=3";
	range = Regex("(vi/|v=|/v/|youtu\\.be/|/embed/)").search(text);
	cout << " group[0] " << range.matchResult().groups()[0] << endl;
	ASSERT(range.start(), ==, 29);
	ASSERT(range.matchResult().groups().size(), ==, 1);
	ASSERT(range.matchResult().groups()[0], ==, "/v/");

	text = "http://youtu.be/.googleapis.com/v/4e_kz79tjb8?version=3";
	range = Regex("(vi/|v=|/v/|youtu\\.be/|/embed/)").search(text);
	ASSERT(range.start(), ==, 7);
	ASSERT(range.matchResult().groups()[0], ==, "youtu.be/");
	ASSERT(range.matchResult().groups()[0], ==, text.substr(range.start(), range.matchResult().length()));
    }
};


/**
 * @brief 
 */
int main(int argc, char *args[]) {

    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new RegexInternalTestCase));
    ts.addTestCase(AutoRef<TestCase>(new RegexTestCase));
    ts.addTestCase(AutoRef<TestCase>(new RegexSearchTest));

    TestReport report(ts.testAll());
    report.validate();
    
    return 0;
}
