#include <iostream>
#include <liboslayer/Lisp.hpp>

using namespace LISP;
using namespace std;

int main(int argc, char *args[]) {

	bool done = false;
	Env env;
	native(env);
	
	while (!done) {
		try {
			repl(env);
		} catch (ExitLispException e) {
			return e.code();
		} catch (ReturnLispException e) {
			cout << " ** ERROR : block '" << e.tag()->toString() << "' not found" << endl;
		} catch (LispException & e) {
			cout << " ** ERROR : " << e.getMessage() << endl;
		}
	}
	
    return 0;
}
