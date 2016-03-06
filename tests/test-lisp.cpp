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

static void test_func() {

	Env env;
	native(env);

	//
	Var proto = parse("(a b c)");
	Arguments args(proto.getList());
	Var input = parse("(1 2 3)");

	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["a"].getInteger(), ==, 1);
	ASSERT(*env["b"].getInteger(), ==, 2);
	ASSERT(*env["c"].getInteger(), ==, 3);

	//
	env = Env();
	native(env);
	proto = parse("(a b c &optional x)");
	args = Arguments(proto.getList());
	input = parse("(1 2 3)");
	
	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["a"].getInteger(), ==, 1);
	ASSERT(*env["b"].getInteger(), ==, 2);
	ASSERT(*env["c"].getInteger(), ==, 3);
	ASSERT(env["x"].nil(), ==, true);

	//
	env = Env();
	native(env);
	proto = parse("(a b c &optional x)");
	args = Arguments(proto.getList());
	input = parse("(1 2 3 4)");

	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["a"].getInteger(), ==, 1);
	ASSERT(*env["b"].getInteger(), ==, 2);
	ASSERT(*env["c"].getInteger(), ==, 3);
	ASSERT(*env["x"].getInteger(), ==, 4);

	//
	env = Env();
	native(env);
	proto = parse("(a b c &optional x &rest y)");
	args = Arguments(proto.getList());
	input = parse("(1 2 3 4)");

	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["a"].getInteger(), ==, 1);
	ASSERT(*env["b"].getInteger(), ==, 2);
	ASSERT(*env["c"].getInteger(), ==, 3);
	ASSERT(*env["x"].getInteger(), ==, 4);
	ASSERT(env["y"].getList().size(), ==, 0);

	//
	env = Env();
	native(env);
	proto = parse("(a b c &optional x &rest y)");
	args = Arguments(proto.getList());
	input = parse("(1 2 3 4 5 6 7)");

	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["a"].getInteger(), ==, 1);
	ASSERT(*env["b"].getInteger(), ==, 2);
	ASSERT(*env["c"].getInteger(), ==, 3);
	ASSERT(*env["x"].getInteger(), ==, 4);
	ASSERT(env["y"].getList().size(), ==, 3);
	ASSERT(*env["y"].getList()[0].getInteger(), ==, 5);
	ASSERT(*env["y"].getList()[1].getInteger(), ==, 6);
	ASSERT(*env["y"].getList()[2].getInteger(), ==, 7);

	//
	env = Env();
	native(env);
	proto = parse("(&optional x &rest y)");
	args = Arguments(proto.getList());
	input = parse("(1 2 3 4 5 6 7)");

	args.mapArguments(env.local(), input.getList());
	ASSERT(*env["x"].getInteger(), ==, 1);
	ASSERT(env["y"].getList().size(), ==, 6);
	ASSERT(*env["y"].getList()[0].getInteger(), ==, 2);
	ASSERT(*env["y"].getList()[1].getInteger(), ==, 3);
	ASSERT(*env["y"].getList()[2].getInteger(), ==, 4);
	ASSERT(*env["y"].getList()[3].getInteger(), ==, 5);
	ASSERT(*env["y"].getList()[4].getInteger(), ==, 6);
	ASSERT(*env["y"].getList()[5].getInteger(), ==, 7);

	//
	env = Env();
	native(env);
	compile("(defun hello (a b c) (+ a 0))", env);
	ASSERT(*compile("(hello 1 2 3)", env).getInteger(), ==, 1);
	compile("(defun hello (a b c) (+ b 0))", env);
	ASSERT(*compile("(hello 1 2 3)", env).getInteger(), ==, 2);
	compile("(defun hello (a b c) (+ c 0))", env);
	ASSERT(*compile("(hello 1 2 3)", env).getInteger(), ==, 3);
	compile("(defun hello (a b c) (+ a b c))", env);
	ASSERT(*compile("(hello 1 2 3)", env).getInteger(), ==, 6);
	compile("(defun hello (a b c &optional x) (if x (+ a b c) (+ a b)))", env);
	ASSERT(*compile("(hello 1 2 3)", env).getInteger(), ==, 3);
	ASSERT(*compile("(hello 1 2 3 t)", env).getInteger(), ==, 6);
	compile("(defun hello (a &optional b &rest c) (list a b c))", env);
	ASSERT(compile("(hello 1 2 3 4 5)", env).getList().size(), ==, 3);
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

static void test_file() {
	Env env;
	native(env);

	compile("(system \"rm hello.txt\")", env);
	ASSERT(compile("(open \"hello.txt\")", env).nil(), ==, true);
	ASSERT(!compile("(open \"hello.txt\" :if-does-not-exist :create)", env).nil(), ==, true);
	ASSERT(compile("(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-line \"hello world\" out) (close out))", env).nil(), ==, true);
	ASSERT(compile("(let ((in (open \"hello.txt\"))) (read-line in))", env).toString(), ==, "hello world");
	ASSERT(compile("(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-string \"hello world\" out) (close out))", env).nil(), ==, true);
	ASSERT(compile("(let ((ret \"\") (in (open \"hello.txt\"))) "
				   "(setq ret (read-line in)) (close in) ret)", env).toString(), ==, "hello world");
}

int main(int argc, char *args[]) {

	try {
		test_func();
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
		test_file();
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

