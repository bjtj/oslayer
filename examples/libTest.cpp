#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

int main(int argc, char *args[]) {

	void (*sym_hello)(void);
	const char * msg = NULL;

	Library lib(".", "hello");
	sym_hello = (void (*)(void))lib.getSymbol("hello");
	sym_hello();

	msg = *(char**)lib.getSymbol("msg");
	cout << "MSG: " << msg << endl;
	
    return 0;
}

