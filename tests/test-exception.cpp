#include <iostream>
#include <liboslayer/os.hpp>
#include "utils.hpp"

using namespace std;
using namespace osl;


DECL_NAMED_EXCEPTION(MyException);

static void test_exception() {

    string err;

    try {
	throw MyException("Error occurred");
    } catch (Exception & e) {
	err = e.toString();
    }

    ASSERT(err, ==, "Error occurred");
}


int main(int argc, char *args[]) {

    test_exception();
    
    return 0;
}
