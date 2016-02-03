#include <iostream>
#include <liboslayer/Lisp.hpp>

using namespace LISP;
using namespace std;


int main(int argc, char *args[]) {

	Env env;
	native(env);
	while (!env.quit()) {
		try {
			repl(env);
		} catch (const char * s) {
			cout << " ** ERROR : " << s << endl;
		}
	}
    
    return 0;
}
