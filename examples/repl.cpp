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
		} catch (LispException & e) {
			cout << " ** ERROR : " << e.getMessage() << endl;
		}
	}
    
    return 0;
}
