#include <iostream>
#include <liboslayer/Properties.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace osl;


#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " expected " << #CMP << " " <<  B << " but " << A << endl; \
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

	string dump = "# comment\nname1= value1\n\nname2= value2";
	props.loadFromString(dump);

	ASSERT(props["name1"], ==, "value1");
	ASSERT(props["name2"], ==, "value2");

	ASSERT(props.hasProperty("x"), ==, false);
	ASSERT(props["x"], ==, "");
	ASSERT(props["x"].empty(), ==, true);
	ASSERT(props.hasProperty("x"), ==, true);

	props.setProperty("int", 10);
	ASSERT(props["int"], ==, "10");

	props.setProperty("float", 1.2f);
	ASSERT(Text::startsWith(props["float"], "1.2"), ==, true);

	ASSERT(props.getFloatProperty("float"), ==, 1.2f);
}

int main(int argc, char *args[]) {

	test_properties();
	
    return 0;
}

