#include <liboslayer/TestSuite.hpp>
#include <liboslayer/StringElements.hpp>

using namespace std;
using namespace osl;


static void test_string_element() {
	
    LinkedStringMap sm;
    ASSERT(sm.contains("a"), ==, false);
    sm["a"] = "A";
    ASSERT(sm.contains("a"), ==, true);

    map<string, string> m;
    m["b"] = "B";
    m["c"] = "C";
    sm = m;
    ASSERT(sm.contains("b"), ==, true);
    ASSERT(sm.contains("c"), ==, true);
    ASSERT(sm["b"], ==, "B");
    ASSERT(sm["c"], ==, "C");

    m["d"] = "D";
    m["e"] = "E";
    sm.put(m);
    ASSERT(sm.contains("b"), ==, true);
    ASSERT(sm.contains("c"), ==, true);
    ASSERT(sm.contains("d"), ==, true);
    ASSERT(sm.contains("e"), ==, true);
    ASSERT(sm["b"], ==, "B");
    ASSERT(sm["c"], ==, "C");
    ASSERT(sm["d"], ==, "D");
    ASSERT(sm["e"], ==, "E");

    LinkedStringMap sm2;
    sm2["x"] = "X";
    sm2["y"] = "Y";

    sm.put(sm2);

    ASSERT(sm.contains("b"), ==, true);
    ASSERT(sm.contains("c"), ==, true);
    ASSERT(sm.contains("d"), ==, true);
    ASSERT(sm.contains("e"), ==, true);
    ASSERT(sm.contains("x"), ==, true);
    ASSERT(sm.contains("y"), ==, true);
    ASSERT(sm["b"], ==, "B");
    ASSERT(sm["c"], ==, "C");
    ASSERT(sm["d"], ==, "D");
    ASSERT(sm["e"], ==, "E");
    ASSERT(sm["x"], ==, "X");
    ASSERT(sm["y"], ==, "Y");

    sm.erase("b");

    ASSERT(sm.contains("b"), ==, false);

    map<string, string> stdm = sm.to_map();
    size_t cnt = 0;
    for (map<string, string>::iterator iter = stdm.begin(); iter != stdm.end(); iter++) {
	cnt++;
	cout << iter->first << " := " << iter->second << endl;
    }

    ASSERT(cnt, ==, sm.size());
}

/**
 * @brief named string list test case
 */
class NamedStringListTestCase : public TestCase {
public:
    NamedStringListTestCase() : TestCase("named-string-list") {
    }
    virtual ~NamedStringListTestCase() {
    }

    virtual void test() {
	Named<StringList> nsl("named");
	ASSERT(nsl.name(), ==, "named");

	ASSERT(nsl.obj().size(), ==, 0);

	nsl.obj().push_back("hello");
	ASSERT(nsl.obj().size(), ==, 1);
	ASSERT(nsl.obj().first(), ==, "hello");

	nsl.obj() += "world";
	ASSERT(nsl.obj().size(), ==, 2);
	ASSERT(nsl.obj().first(), ==, "hello");
	ASSERT(nsl.obj().last(), ==, "world");

	vector<string> strs;
	strs.push_back("a");
	strs.push_back("b");
	strs.push_back("c");

	nsl.obj() = strs;
	ASSERT(nsl.obj().size(), ==, 3);
	ASSERT(nsl.obj().first(), ==, "a");
	ASSERT(nsl.obj().last(), ==, "c");

	vector<string> strs2;
	strs2.push_back("d");
	strs2.push_back("e");
	strs2.push_back("f");

	nsl.obj() += strs2;
	ASSERT(nsl.obj().size(), ==, 6);
	ASSERT(nsl.obj().first(), ==, "a");
	ASSERT(nsl.obj().last(), ==, "f");
    }
};

class LinkedStringListMapTestCase : public TestCase {
public:
    LinkedStringListMapTestCase() : TestCase("linked-string-list-map") {}
    virtual ~LinkedStringListMapTestCase() {}

    virtual void test() {
	LinkedStringListMap m;
	m["WWW-Authenticate"] += "NTLM";
	m["WWW-Authenticate"] += "Basic realm=\"Redmine Authentication\"";

	ASSERT(m.size(), ==, 1);

	ASSERT(m["WWW-Authenticate"].first(), ==, "NTLM");
	ASSERT(m["WWW-Authenticate"].last(), ==, "Basic realm=\"Redmine Authentication\"");

	map<string, string> x;
	x["a"] = "A";
	x["b"] = "B";

	m = x;
	ASSERT(m.size(), ==, 2);
	ASSERT(m["a"].first(), ==, "A");
	ASSERT(m["b"].first(), ==, "B");

	LinkedStringMap s;
	s["x"] = "X";
	s["y"] = "Y";

	m = s;
	ASSERT(m.size(), ==, 2);
	ASSERT(m["x"].first(), ==, "X");
	ASSERT(m["y"].first(), ==, "Y");

	m += x;
	ASSERT(m.size(), ==, 4);
	ASSERT(m["a"].first(), ==, "A");
	ASSERT(m["b"].first(), ==, "B");
	ASSERT(m["x"].first(), ==, "X");
	ASSERT(m["y"].first(), ==, "Y");

	m.erase("x");
	ASSERT(m.size(), ==, 3);
	ASSERT(m["a"].first(), ==, "A");
	ASSERT(m["b"].first(), ==, "B");
	ASSERT(m["y"].first(), ==, "Y");
    }
};

class LinkedStringListMap2TestCase : public TestCase {
public:
    LinkedStringListMap2TestCase() : TestCase("linked-string-list-map2") {}
    virtual ~LinkedStringListMap2TestCase() {}

    virtual void test() {
	LinkedStringListMap m;
	m["Host"].clearSet("123");
	ASSERT(m["Host"].first(), ==, "123");
	ASSERT(m["Host"].first(""), ==, "123");
	m["Host"].first() = "456";
	ASSERT(m["Host"].first(), ==, "456");
	ASSERT(m[0].obj().first(), ==, "456");
	ASSERT(m[0].obj().first(""), ==, "456");
    }
};

int main(int argc, char *args[]) {

    TestSuite ts;
    ts.addTestCase(AutoRef<TestCase>(new NamedStringListTestCase));
    ts.addTestCase(AutoRef<TestCase>(new LinkedStringListMapTestCase));
    ts.addTestCase(AutoRef<TestCase>(new LinkedStringListMap2TestCase));

    test_string_element();

    TestReport report(ts.testAll());
    report.validate();
    
    return 0;
}
