#include <iostream>
#include <liboslayer/Properties.hpp>

using namespace std;
using namespace UTIL;

#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	}

static void test_properties() {
	Properties props;
	props["a"] = "A";
	props["b"] = "B";
	props["c"] = "C";
	props["d"] = "D";
	props["e"] = "E";

	map<string, string> m = props.toStandardMap();
	ASSERT(m["a"], ==, "A");
	ASSERT(m["b"], ==, "B");
	ASSERT(m["c"], ==, "C");
	ASSERT(m["d"], ==, "D");
	ASSERT(m["e"], ==, "E");
}

int main(int argc, char *args[]) {

	test_properties();
	
    return 0;
}

