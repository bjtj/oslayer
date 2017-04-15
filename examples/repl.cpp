#include <iostream>
#include <liboslayer/Lisp.hpp>

using namespace LISP;
using namespace std;

int main(int argc, char *args[]) {

	if (argc > 1 && string (args[1]) == "--debug") {
		Env::setDebug(true);
		Var::setDebug(true);
	}

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
			cout << " ** ERROR : " << e.toString() << endl;
		}
	}
	
    return 0;
}
