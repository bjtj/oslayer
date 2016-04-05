#include "utils.hpp"
#include <liboslayer/StringElement.hpp>

using namespace std;
using namespace UTIL;

static void test_string_element() {
	
	LinkedStringMap sm;
	ASSERT(sm.has("a"), ==, false);
	sm["a"] = "A";
	ASSERT(sm.has("a"), ==, true);

	map<string, string> m;
	m["b"] = "B";
	m["c"] = "C";
	sm = m;
	ASSERT(sm.has("b"), ==, true);
	ASSERT(sm.has("c"), ==, true);
	ASSERT(sm["b"], ==, "B");
	ASSERT(sm["c"], ==, "C");

	m["d"] = "D";
	m["e"] = "E";
	sm.append(m);
	ASSERT(sm.has("b"), ==, true);
	ASSERT(sm.has("c"), ==, true);
	ASSERT(sm.has("d"), ==, true);
	ASSERT(sm.has("e"), ==, true);
	ASSERT(sm["b"], ==, "B");
	ASSERT(sm["c"], ==, "C");
	ASSERT(sm["d"], ==, "D");
	ASSERT(sm["e"], ==, "E");

	LinkedStringMap sm2;
	sm2["x"] = "X";
	sm2["y"] = "Y";

	sm.append(sm2);

	ASSERT(sm.has("b"), ==, true);
	ASSERT(sm.has("c"), ==, true);
	ASSERT(sm.has("d"), ==, true);
	ASSERT(sm.has("e"), ==, true);
	ASSERT(sm.has("x"), ==, true);
	ASSERT(sm.has("y"), ==, true);
	ASSERT(sm["b"], ==, "B");
	ASSERT(sm["c"], ==, "C");
	ASSERT(sm["d"], ==, "D");
	ASSERT(sm["e"], ==, "E");
	ASSERT(sm["x"], ==, "X");
	ASSERT(sm["y"], ==, "Y");

	sm.erase("b");

	ASSERT(sm.has("b"), ==, false);

	map<string, string> stdm = sm.toStdMap();
	size_t cnt = 0;
	for (map<string, string>::iterator iter = stdm.begin(); iter != stdm.end(); iter++) {
		cnt++;
		cout << iter->first << " := " << iter->second << endl;
	}

	ASSERT(cnt, ==, sm.size());
}

int main(int argc, char *args[]) {

	test_string_element();
    
    return 0;
}
