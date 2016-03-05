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

static void test_logic() {
	Env env;
	native(env);
	ASSERT(compile("(not t)", env).nil(), ==, true);
	ASSERT(compile("(not nil)", env).nil(), ==, false);
	ASSERT(compile("(or 1 1)", env).nil(), ==, false);
	ASSERT(compile("(and t nil)", env).nil(), ==, true);
	ASSERT(compile("(or t nil)", env).nil(), ==, false);
	ASSERT(compile("(or (and t nil) t))", env).nil(), ==, false);
	ASSERT(compile("(and (or nil t) nil)", env).nil(), ==, true);
}

static void test_type() {
	Var var(false);
	ASSERT(var.nil(), ==, true);
}

static void test_scope() {
	Env env;
	native(env);

	ASSERT(compile("(let ((ret \"\")) ret)", env).toString(), ==, "");
	ASSERT(compile("(let ((ret \"\")) (setq ret (list 1 2 3)) (print ret) ret)", env).getTypeString(), ==, "LIST");
}

static void test_cond() {
	Env env;
	native(env);

	ASSERT(*compile("(if nil 1 0)", env).getInteger(), ==, 0);
	ASSERT(*compile("(if t 1 0)", env).getInteger(), ==, 1);
	ASSERT(*compile("(when t 1)", env).getInteger(), ==, 1);
	ASSERT(compile("(when nil 1)", env).nil(), ==, true);
	ASSERT(compile("(unless t 1)", env).nil(), ==, true);
	ASSERT(*compile("(unless nil 1)", env).getInteger(), ==, 1);
	compile("(setq a 5)", env);
	ASSERT(*compile("(cond ((= a 5) 1) (t \"default\"))", env).getInteger(), ==, 1);
	ASSERT(compile("(cond ((string= a \"hack\") \"foo\") (t \"default\"))", env).toString(), ==, "default");
}

static void test_loop() {
	Env env;
	native(env);

	compile("(setq num 0)", env);
	ASSERT(compile("(while (< num 4) (progn (print num) (setq num (+ num 1)))))", env).nil(), ==, true);
	ASSERT(compile("(dolist (x (list 1 2 3)) (print x))", env).nil(), ==, true);
	ASSERT(compile("(dotimes (i 10) (print i))", env).nil(), ==, true);
}

static void test_read() {

	Env env;
	native(env);

	ASSERT(BufferedCommandReader::testComplete("3\n"), ==, 1);

	BufferedCommandReader reader;
	vector<string> lines;
	
	lines.push_back("3");
	lines.push_back("(+\n");
	lines.push_back("1 2\n");
	lines.push_back(") (* 1 3)\n");

	ASSERT(BufferedCommandReader::testComplete(" 3\n"), >, 0);	

	ASSERT(reader.read("3\n"), ==, 1);
	ASSERT(reader.getCommands().size(), ==, 1);
	
	for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
		if (reader.read(*iter) > 0) {
			vector<string> & commands = reader.getCommands();
			for (vector<string>::iterator cmd = commands.begin(); cmd != commands.end(); cmd++) {
				Var ret = compile(*cmd, env);
				ASSERT(*ret.getInteger(), ==, 3);
			}
			reader.clearCommands();
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

	ASSERT(compile("(string-prefix-p \".bashrc\" \".\")", env).nil(), ==, false);
	ASSERT(compile("(string-prefix-p \"bashrc\" \".\")", env).nil(), ==, true);

	ASSERT(compile("(string-append \"hello\" \" world\")", env).toString(), ==, "hello world");

	ASSERT(*compile("(string-length \"hello world\")", env).getInteger(), ==, strlen("hello world"));
}

static void test_arithmetic() {
	Env env;
	native(env);
	ASSERT(!compile("(= 1 1)", env).nil(), ==, true);
	ASSERT(!compile("(= 2 1)", env).nil(), ==, false);
	ASSERT(!compile("(= 1 2)", env).nil(), ==, false);
	ASSERT(!compile("(< 1 4)", env).nil(), ==, true);
	ASSERT(!compile("(> 1 4)", env).nil(), ==, false);
	ASSERT(!compile("(<= 1 1)", env).nil(), ==, true);
	ASSERT(!compile("(<= 1 4)", env).nil(), ==, true);
	ASSERT(!compile("(<= 4 1)", env).nil(), ==, false);
	ASSERT(!compile("(>= 1 1)", env).nil(), ==, true);
	ASSERT(!compile("(>= 1 4)", env).nil(), ==, false);
	ASSERT(!compile("(>= 4 1)", env).nil(), ==, true);
}

static void test_list() {

	Env env;
	native(env);
	ASSERT(compile("(remove-if (lambda (x) (= x 1)) (list 1 2 1 3))", env).getList().size(), ==, 2);
}

static void test_algorithm() {
	Env env;
	native(env);
	ASSERT(*compile("(map 'list (lambda (x) (+ x 1)) (list 1 2 3))", env).getList()[0].getInteger(),
		   ==, 2);
	ASSERT(*compile("(map 'list (lambda (x) (+ x 1)) (list 1 2 3))", env).getList()[1].getInteger(),
		   ==, 3);
	ASSERT(*compile("(map 'list (lambda (x) (+ x 1)) (list 1 2 3))", env).getList()[2].getInteger(),
		   ==, 4);
}

int main(int argc, char *args[]) {

	try {
		test_type();
		test_scope();
		test_logic();
		test_cond();
		test_loop();
		test_read();
		test_string();
		test_list();
		test_arithmetic();
		test_algorithm();
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
