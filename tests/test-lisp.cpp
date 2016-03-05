#include <iostream>
#include <liboslayer/Lisp.hpp>

using namespace std;
using namespace LISP;

#define ASSERT(A,CMP,B) if (!(A CMP B)) {								\
		cerr << #A <<  " should be " << #CMP << " " <<  B << " but " << A << endl; \
		exit(1);														\
	}

static Var compile(const string & cmd, Env & env) {
	Var var = parse(cmd);
	return eval(var, env);
}

static void test_read() {

	Env env;
	native(env);

	BufferedCommandReader reader;
	vector<string> lines;
	lines.push_back("(+\n");
	lines.push_back("1 2\n");
	lines.push_back(") (* 1 3)\n");
	for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
		if (reader.read(*iter) > 0) {
			vector<string> & commands = reader.getCommands();
			for (vector<string>::iterator cmd = commands.begin(); cmd != commands.end(); cmd++) {
				Var ret = compile(*cmd, env);
				cout << ret.toString() << endl;
				ASSERT(*ret.getInteger(), ==, 3);
			}
			reader.clear();
		}
	}
}

static void test_string() {
	Env env;
	native(env);
	Var ret = compile("(setq hello \"Hello World\")", env);

	ASSERT(ret.toString(), ==, "Hello World");
	ASSERT(env["hello"].toString(), ==, "Hello World");

	ret = compile("(enough-namestring \"/www/html/foo/bar/baz.html\" \"/www/\")", env);
	ASSERT(ret.toString(), ==, "html/foo/bar/baz.html");

	ASSERT(*compile("1", env).getInteger(), ==, 1);

	ASSERT(compile("(string-prefix-p \".bashrc\" \".\")", env).getBoolean(), ==, true);
	ASSERT(compile("(string-prefix-p \"bashrc\" \".\")", env).getBoolean(), !=, true);
}

int main(int argc, char *args[]) {

	try {
		test_read();
		test_string();
	} catch (const char * e) {
		cout << e << endl;
		exit(1);
	} catch (const string & e) {
		cout << e << endl;
		exit(1);
	} catch (...) {
		cout << "error..." << endl;
		exit(1);
	}
	
    return 0;
}

