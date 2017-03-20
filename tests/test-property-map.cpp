#include "utils.hpp"
#include <liboslayer/PropertyMap.hpp>

using namespace std;
using namespace UTIL;

static void test_property_map() {
	
	PropertyMap pm;

	pm["a"] = "A";
	pm["b"] = "B";

	ASSERT(pm["a"], ==, "A");
	ASSERT(pm["b"], ==, "B");
	
	ASSERT(pm[0].key(), ==, "a");
	ASSERT(pm[1].key(), ==, "b");
	ASSERT(pm[0].value(), ==, "A");
	ASSERT(pm[1].value(), ==, "B");

	pm.get("a").attr("a-a") = "A-A";
	pm.get("a").attr("a-b") = "A-B";

	ASSERT(pm.get("a").attr("a-a"), ==, "A-A");
	ASSERT(pm.get("a").attr("a-b"), ==, "A-B");

	pm.erase("a");
	ASSERT(pm["a"], ==, "");
}

int main(int argc, char *args[]) {

	test_property_map();
    
    return 0;
}
