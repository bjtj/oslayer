#include <liboslayer/TestSuite.hpp>
#include <liboslayer/StringElements.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

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
	sm.append(m);
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

	sm.append(sm2);

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

	map<string, string> stdm = sm.toStdMap();
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
		NamedStringList nsl("named");
		ASSERT(nsl.name(), ==, "named");

		ASSERT(nsl.size(), ==, 0);

		nsl = "hello";
		ASSERT(nsl.size(), ==, 1);
		ASSERT(nsl.first(), ==, "hello");

		nsl += "world";
		ASSERT(nsl.size(), ==, 2);
		ASSERT(nsl.first(), ==, "hello");
		ASSERT(nsl.last(), ==, "world");

		vector<string> strs;
		strs.push_back("a");
		strs.push_back("b");
		strs.push_back("c");

		nsl = strs;
		ASSERT(nsl.size(), ==, 3);
		ASSERT(nsl.first(), ==, "a");
		ASSERT(nsl.last(), ==, "c");

		vector<string> strs2;
		strs2.push_back("d");
		strs2.push_back("e");
		strs2.push_back("f");

		nsl += strs2;
		ASSERT(nsl.size(), ==, 6);
		ASSERT(nsl.first(), ==, "a");
		ASSERT(nsl.last(), ==, "f");
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


int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new NamedStringListTestCase));
	ts.addTestCase(AutoRef<TestCase>(new LinkedStringListMapTestCase));

	test_string_element();

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
