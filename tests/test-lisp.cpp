#include <iostream>
#include <liboslayer/Lisp.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace LISP;

#define _VAR GCRef<Var> 

static void test_comment() {
	string text = "hello ; comment";
	ASSERT(BufferedCommandReader::eliminateComment(text), ==, "hello ");
	text = "(defun hello () ;sample hello program\n"
		"(print \"hello world\")) ; print greeting";
	ASSERT(BufferedCommandReader::eliminateComment(text), ==, "(defun hello () \n"
		   "(print \"hello world\")) ");
	text = "\" ; \"";
	ASSERT(BufferedCommandReader::eliminateComment(text), ==, "\" ; \"");

	Env env;
	native(env);
	ASSERT(compile(env, "(format nil \"hello\") ; comment")->toString(), ==, "hello");
	ASSERT(BufferedCommandReader::eliminateComment("(format nil ;comment\n"
											  "\"hello\")"), ==, "(format nil \n"
		   "\"hello\")");
	ASSERT(compile(env, "(format nil\n"
				   "\"hello\")")->toString(), ==, "hello");
	ASSERT(compile(env, "(format nil ; comment \n"
				   "\"hello\")")->toString(), ==, "hello");
}

static void test_subseq() {
	Env env;
	native(env);
	ASSERT(compile(env, "(subseq (list 1 2 3) 0 2)")->getList().size(), ==, 2);
}

static void test_ref() {
	Env env;
	native(env);

	ASSERT(*compile(env, "(setq a (car (list 1 2 3)))")->getInteger(), ==, 1);
	ASSERT(*env["a"]->getInteger(), ==, 1);
	ASSERT(*compile(env, "(setq b (cdr (list 1 2 3)))")->getList()[0]->getInteger(), ==, 2);
	ASSERT(*env["b"]->getList()[1]->getInteger(), ==, 3);

	compile(env, "(setq lst (list (list 1 2) (list 3 4)))");
	ASSERT(*compile(env, "(car (car lst))")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(car (cdr (car lst)))")->getInteger(), ==, 2);

	ASSERT(compile(env, "(car (cdr lst))")->getList().size(), ==, 2);
	ASSERT(*compile(env, "(car (cdr lst))")->getList()[0]->getInteger(), ==, 3);
	ASSERT(*compile(env, "(car (cdr lst))")->getList()[1]->getInteger(), ==, 4);
	
	ASSERT(*compile(env, "(car (car (cdr lst)))")->getInteger(), ==, 3);
	ASSERT(*compile(env, "(car (cdr (car (cdr lst))))")->getInteger(), ==, 4);
}

static void test_quote() {
	Env env;
	native(env);

	ASSERT(compile(env, "'(1 2 3)")->getList().size(), ==, 3);
	ASSERT(*compile(env, "'(1 2 3)")->getList()[0]->getInteger(), ==, 1);
	ASSERT(*compile(env, "'(1 2 3)")->getList()[1]->getInteger(), ==, 2);
	ASSERT(*compile(env, "'(1 2 3)")->getList()[2]->getInteger(), ==, 3);

	ASSERT(compile(env, "`(1 2 3)")->getList().size(), ==, 3);
	ASSERT(*compile(env, "`(1 2 3)")->getList()[0]->getInteger(), ==, 1);
	ASSERT(*compile(env, "`(1 2 3)")->getList()[1]->getInteger(), ==, 2);
	ASSERT(*compile(env, "`(1 2 3)")->getList()[2]->getInteger(), ==, 3);

	ASSERT(compile(env, "`(1 2 ,(+ 2 3))")->getList().size(), ==, 3);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->getList()[0]->getInteger(), ==, 1);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->getList()[1]->getInteger(), ==, 2);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->getList()[2]->getInteger(), ==, 5);
}

static void test_setf() {
	Env env;
	native(env);

	compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))");
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList().size(), ==, 2);

	compile(env, "(setq n 2)");
	ASSERT(*env["n"]->getInteger(), ==, 2);
	compile(env, "(setf n 7)");
	ASSERT(*env["n"]->getInteger(), ==, 7);

	compile(env, "(setq lst (list 1 2 3))");
	compile(env, "(setf (car lst) 7)");
	
	ASSERT(env["lst"]->getList().size(), ==, 3);
	ASSERT(*env["lst"]->getList()[0]->getInteger(), ==, 7);
	ASSERT(*env["lst"]->getList()[1]->getInteger(), ==, 2);
	ASSERT(*env["lst"]->getList()[2]->getInteger(), ==, 3);

	compile(env, "(setf (subseq lst 0 1) (list 9))");
	ASSERT(*env["lst"]->getList()[0]->getInteger(), ==, 9);
	ASSERT(*env["lst"]->getList()[1]->getInteger(), ==, 2);
	ASSERT(*env["lst"]->getList()[2]->getInteger(), ==, 3);

	ASSERT(compile(env, "(setf (list 1 2) (list 4 5))")->getList().size(), ==, 2);
	ASSERT(*compile(env, "(setf (list 1 2) (list 4 5))")->getList()[0]->getInteger(), ==, 4);
	ASSERT(*compile(env, "(setf (list 1 2) (list 4 5))")->getList()[1]->getInteger(), ==, 5);
	ASSERT(*compile(env, "(car (setf (list 1 2) (list 4 5)))")->getInteger(), ==, 4);
	ASSERT(*compile(env, "(car (cdr (setf (list 1 2) (list 4 5))))")->getInteger(), ==, 5);

	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList().size(), ==, 2);
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList()[0]->getType(),
		   ==, Var::INTEGER);
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList()[1]->getType(),
		   ==, Var::INTEGER);
	ASSERT(*compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList()[0]->getInteger(),
		   ==, 4);
	ASSERT(*compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->getList()[1]->getInteger(),
		   ==, 5);
	
}

static void test_func() {

	{
		Env env;
		native(env);

		_VAR proto = parse(env, "(a b c)");
		Arguments args(proto->getList());
		_VAR input = parse(env, "(1 2 3)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);

		env.clear();

		env = Env();
		native(env);
		proto = parse(env, "(a b c &optional x)");
		args = Arguments(proto->getList());
		input = parse(env, "(1 2 3)");
	
		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(env["x"]->isNil(), ==, true);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x)");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3 4)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(*env["x"]->getInteger(), ==, 4);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional (x 1))");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(*env["x"]->getInteger(), ==, 1);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional (x 1))");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3 4)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(*env["x"]->getInteger(), ==, 4);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x &rest y)");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3 4)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(*env["x"]->getInteger(), ==, 4);
		ASSERT(env["y"]->getList().size(), ==, 0);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x &rest y)");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3 4 5 6 7)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["a"]->getInteger(), ==, 1);
		ASSERT(*env["b"]->getInteger(), ==, 2);
		ASSERT(*env["c"]->getInteger(), ==, 3);
		ASSERT(*env["x"]->getInteger(), ==, 4);
		ASSERT(env["y"]->getList().size(), ==, 3);
		ASSERT(*env["y"]->getList()[0]->getInteger(), ==, 5);
		ASSERT(*env["y"]->getList()[1]->getInteger(), ==, 6);
		ASSERT(*env["y"]->getList()[2]->getInteger(), ==, 7);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(&optional x &rest y)");
		Arguments args = Arguments(proto->getList());
		_VAR input = parse(env, "(1 2 3 4 5 6 7)");

		args.mapArguments(env, env.local(), input->getList());
		ASSERT(*env["x"]->getInteger(), ==, 1);
		ASSERT(env["y"]->getList().size(), ==, 6);
		ASSERT(*env["y"]->getList()[0]->getInteger(), ==, 2);
		ASSERT(*env["y"]->getList()[1]->getInteger(), ==, 3);
		ASSERT(*env["y"]->getList()[2]->getInteger(), ==, 4);
		ASSERT(*env["y"]->getList()[3]->getInteger(), ==, 5);
		ASSERT(*env["y"]->getList()[4]->getInteger(), ==, 6);
		ASSERT(*env["y"]->getList()[5]->getInteger(), ==, 7);
	}

	{
		//
		Env env;
		native(env);
		compile(env, "(defun hello (a b c) (+ a 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->getInteger(), ==, 1);
		compile(env, "(defun hello (a b c) (+ b 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->getInteger(), ==, 2);
		compile(env, "(defun hello (a b c) (+ c 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->getInteger(), ==, 3);
		compile(env, "(defun hello (a b c) (+ a b c))");
		ASSERT(*compile(env, "(hello 1 2 3)")->getInteger(), ==, 6);
		compile(env, "(defun hello (a b c &optional x) (if x (+ a b c) (+ a b)))");
		ASSERT(*compile(env, "(hello 1 2 3)")->getInteger(), ==, 3);
		ASSERT(*compile(env, "(hello 1 2 3 t)")->getInteger(), ==, 6);
		compile(env, "(defun hello (a &optional b &rest c) (list a b c))");
		ASSERT(compile(env, "(hello 1 2 3 4 5)")->getList().size(), ==, 3);
	}

	{
		//
		Env env;
		native(env);
		compile(env, "(setq str \"\")");
		compile(env, "(defun wr (x) (setq str x))");
		compile(env, "(dolist (x (list 1 2 3)) (wr x))");
	}
}

static void test_func_more() {
	Env env;
	native(env);

	compile(env, "(defun hello () (print \"hello\"))");

	string err;
	try {
		compile(env, "(funcall hello)");
	} catch (LispException & e) {
		err = e.getMessage();
	}
	ASSERT(err.empty(), ==, false);
	ASSERT(compile(env, "(funcall (quote hello))")->toString(), ==, "hello");
	ASSERT(compile(env, "(funcall (function hello))")->toString(), ==, "hello");
}

static void test_logic() {
	Env env;
	native(env);
	ASSERT(compile(env, "(not t)")->isNil(), ==, true);
	ASSERT(compile(env, "(not nil)")->isNil(), ==, false);
	ASSERT(compile(env, "(or 1 1)")->isNil(), ==, false);
	ASSERT(compile(env, "(and t nil)")->isNil(), ==, true);
	ASSERT(compile(env, "(or t nil)")->isNil(), ==, false);
	ASSERT(compile(env, "(or (and t nil) t))")->isNil(), ==, false);
	ASSERT(compile(env, "(and (or nil t) nil)")->isNil(), ==, true);
}

static void test_type() {

	Env env;
	native(env);
	
	Var var(false);
	ASSERT(var.isNil(), ==, true);

	ASSERT(compile(env, "(symbolp (quote x))")->isNil(), ==, false);
	ASSERT(compile(env, "(listp (list 1 2 3))")->isNil(), ==, false);
	ASSERT(compile(env, "(booleanp t)")->isNil(), ==, false);
	ASSERT(compile(env, "(integerp 1)")->isNil(), ==, false);
	ASSERT(compile(env, "(floatp 1.0)")->isNil(), ==, false);
	ASSERT(compile(env, "(stringp \"hello\")")->isNil(), ==, false);
	compile(env, "(system \"touch xxx\")");
	compile(env, "(setq *f* (open \"xxx\"))");
	ASSERT(compile(env, "(streamp *f*)")->isNil(), ==, false);
}

static void test_scope() {
	Env env;
	native(env);

	ASSERT(compile(env, "(let ((ret \"\")) ret)")->toString(), ==, "");
	ASSERT(compile(env, "(let ((ret \"\")) (setq ret (list 1 2 3)) (print ret) ret)")->getTypeString(), ==, "LIST");
}

static void test_cond() {
	Env env;
	native(env);

	ASSERT(*compile(env, "(if nil 1 0)")->getInteger(), ==, 0);
	ASSERT(*compile(env, "(if t 1 0)")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(when t 1)")->getInteger(), ==, 1);
	ASSERT(compile(env, "(when nil 1)")->isNil(), ==, true);
	ASSERT(compile(env, "(unless t 1)")->isNil(), ==, true);
	ASSERT(*compile(env, "(unless nil 1)")->getInteger(), ==, 1);
	compile(env, "(setq a 5)");
	ASSERT(*compile(env, "(cond ((= a 5) 1) (t \"default\"))")->getInteger(), ==, 1);
	ASSERT(compile(env, "(cond ((string= a \"hack\") \"foo\") (t \"default\"))")->toString(), ==, "default");
}

static void test_loop() {
	Env env;
	native(env);

	compile(env, "(setq num 0)");
	ASSERT(compile(env, "(while (< num 4) (progn (print num) (setq num (+ num 1)))))")->isNil(), ==, true);
	ASSERT(compile(env, "(dolist (x (list 1 2 3)) (print x))")->isNil(), ==, true);
	ASSERT(compile(env, "(dotimes (i 10) (print i))")->isNil(), ==, true);

	compile(env, "(setq *lst* (list (list 1 2) (list 3 4)))");
	compile(env, "(dolist (x *lst*) (print x))");
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
				_VAR ret = compile(env, *cmd);
				ASSERT(*ret->getInteger(), ==, 3);
			}
			reader.clearCommands();
		}
	}
}

static void test_string() {
	Env env;
	native(env);
	_VAR ret = compile(env, "(setq hello \"Hello World\")");

	ASSERT(ret->toString(), ==, "Hello World");
	ASSERT(env["hello"]->toString(), ==, "Hello World");

	ret = compile(env, "(enough-namestring \"/www/html/foo/bar/baz.html\" \"/www/\")");
	ASSERT(ret->toString(), ==, "html/foo/bar/baz.html");

	ASSERT(*compile(env, "1")->getInteger(), ==, 1);

	ASSERT(compile(env, "(string-prefix-p \".bashrc\" \".\")")->isNil(), ==, false);
	ASSERT(compile(env, "(string-prefix-p \"bashrc\" \".\")")->isNil(), ==, true);

	ASSERT(compile(env, "(string-append \"hello\" \" world\")")->toString(), ==, "hello world");

	ASSERT(*compile(env, "(string-length \"hello world\")")->getInteger(), ==, strlen("hello world"));
}

static void test_format() {
	Env env;
	native(env);

	ASSERT(compile(env, "(format nil \"hello world\")")->toString(), ==, "hello world");
	ASSERT(compile(env, "(format nil \"hello, ~a?\" \"friend\")")->toString(), ==, "hello, friend?");

	string err;
	try {
		compile(env, "(format nil \"hello ~a\")");
	} catch (Exception & e) {
		err = e.getMessage();
	}
	ASSERT(err, ==, "out of bound");
}

static void test_arithmetic() {
	Env env;
	native(env);
	ASSERT(!compile(env, "(= 1 1)")->isNil(), ==, true);
	ASSERT(!compile(env, "(= 2 1)")->isNil(), ==, false);
	ASSERT(!compile(env, "(= 1 2)")->isNil(), ==, false);
	ASSERT(!compile(env, "(< 1 4)")->isNil(), ==, true);
	ASSERT(!compile(env, "(> 1 4)")->isNil(), ==, false);
	ASSERT(!compile(env, "(<= 1 1)")->isNil(), ==, true);
	ASSERT(!compile(env, "(<= 1 4)")->isNil(), ==, true);
	ASSERT(!compile(env, "(<= 4 1)")->isNil(), ==, false);
	ASSERT(!compile(env, "(>= 1 1)")->isNil(), ==, true);
	ASSERT(!compile(env, "(>= 1 4)")->isNil(), ==, false);
	ASSERT(!compile(env, "(>= 4 1)")->isNil(), ==, true);

	ASSERT(!compile(env, "(= 1 1.0)")->isNil(), ==, true);
	ASSERT(!compile(env, "(= 2 1.0)")->isNil(), ==, false);
	ASSERT(!compile(env, "(= 1 2.0)")->isNil(), ==, false);
	ASSERT(!compile(env, "(< 1 4.0)")->isNil(), ==, true);
	ASSERT(!compile(env, "(> 1 4.0)")->isNil(), ==, false);
	ASSERT(!compile(env, "(<= 1 1.0)")->isNil(), ==, true);
	ASSERT(!compile(env, "(<= 1 4.0)")->isNil(), ==, true);
	ASSERT(!compile(env, "(<= 4 1.0)")->isNil(), ==, false);
	ASSERT(!compile(env, "(>= 1 1.0)")->isNil(), ==, true);
	ASSERT(!compile(env, "(>= 1 4.0)")->isNil(), ==, false);
	ASSERT(!compile(env, "(>= 4 1.0)")->isNil(), ==, true);

	ASSERT(*compile(env, "(* 4 1.2)")->getFloat(), ==, 4.8);
}

static void test_list() {

	Env env;
	native(env);
	ASSERT(compile(env, "(remove-if (lambda (x) (= x 1)) (list 1 2 1 3))")->getList().size(), ==, 2);
	compile(env, "(setq *lst* (list 1 2 3))");
	ASSERT(*compile(env, "(car *lst*)")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(car (list 1 2 3))")->getInteger(), ==, 1);
	ASSERT((compile(env, "(cdr (list 1 2 3))"))->getList().size(), ==, 2);
	ASSERT(*compile(env, "(cdr (list 1 2 3))")->getList()[0]->getInteger(), ==, 2);
	ASSERT(*compile(env, "(cdr (list 1 2 3))")->getList()[1]->getInteger(), ==, 3);
	ASSERT(*compile(env, "(nth 0 (list 1 2 3))")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(nth 1 (list 1 2 3))")->getInteger(), ==, 2);
	ASSERT(*compile(env, "(nth 2 (list 1 2 3))")->getInteger(), ==, 3);
	ASSERT(compile(env, "(nth 10 (list 1 2 3))")->isNil(), ==, true);
	ASSERT(compile(env, "(nthcdr 0 (list 1 2 3))")->getList().size(), ==, 3);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->getList()[0]->getInteger(), ==, 1);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->getList()[1]->getInteger(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->getList()[2]->getInteger(), ==, 3);
	ASSERT(compile(env, "(nthcdr 1 (list 1 2 3))")->getList().size(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 1 (list 1 2 3))")->getList()[0]->getInteger(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 1 (list 1 2 3))")->getList()[1]->getInteger(), ==, 3);
	ASSERT(compile(env, "(nthcdr 2 (list 1 2 3))")->getList().size(), ==, 1);
	ASSERT(*compile(env, "(nthcdr 2 (list 1 2 3))")->getList()[0]->getInteger(), ==, 3);
	ASSERT(compile(env, "(nthcdr 10 (list 1 2 3))")->isNil(), ==, true);

	compile(env, "(setq *lst* (list (list \"name\" \"steve\") (list \"age\" \"23\")))");
	ASSERT((compile(env, "(car *lst*)"))->getList().size(), ==, 2);
	ASSERT((compile(env, "(car (car *lst*))"))->toString(), ==, "name");
	ASSERT((compile(env, "(car (cdr (car *lst*)))"))->toString(), ==, "steve");
}

static void test_cons() {
	Env env;
	native(env);

	compile(env, "(cons 1 2)");
	ASSERT(compile(env, "(cons 1 2)")->toString(), ==, "(1 2)");
	ASSERT(compile(env, "(cons 1 (cons 2 3)))")->toString(), ==, "(1 2 3)");
	ASSERT(compile(env, "(cons 1 (cons 2 (cons 3 4))))")->toString(), ==, "(1 2 3 4)");

	ASSERT(compile(env, "(car (cons 1 2))")->getType(), ==, Var::INTEGER);
	ASSERT(*compile(env, "(car (cons 1 2))")->getInteger(), ==, 1);
	ASSERT(compile(env, "(cdr (cons 1 2))")->getType(), ==, Var::LIST);
	ASSERT(*compile(env, "(cdr (cons 1 2))")->getList()[0]->getInteger(), ==, 2);
}

static void test_algorithm() {
	Env env;
	native(env);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->getList()[0]->getInteger(),
		   ==, 2);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->getList()[1]->getInteger(),
		   ==, 3);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->getList()[2]->getInteger(),
		   ==, 4);
	ASSERT(compile(env, "(sort (list 1 2 3 4) (lambda (a b) (> a b)))")->getList().size(), ==, 4);

	string lst = "1 2 3 4";
	string cmp = "(lambda (a b) (> a b))";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[3]->getInteger(),
		   ==, 4);

	lst = "4 3 2 1";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[3]->getInteger(),
		   ==, 4);

	lst = "2 4 1 3";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[3]->getInteger(),
		   ==, 4);

	lst = "2 4 1 3";
	cmp = "(lambda (a b) (< a b))";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[0]->getInteger(),
		   ==, 4);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[1]->getInteger(),
		   ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[2]->getInteger(),
		   ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->getList()[3]->getInteger(),
		   ==, 1);
}

static void test_file() {
	Env env;
	native(env);

	compile(env, "(system \"rm hello.txt\")");
	//ASSERT(compile(env, "(open \"hello.txt\")")->isNil(), ==, true);
	ASSERT(compile(env, "(open \"hello.txt\" :if-does-not-exist nil)")->isNil(), ==, true);
	ASSERT(!compile(env, "(open \"hello.txt\" :if-does-not-exist :create)")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-line \"hello world\" out) (close out))")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((in (open \"hello.txt\"))) (read-line in))")->toString(), ==, "hello world");
	ASSERT(compile(env, "(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-string \"hello world\" out) (close out))")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((ret \"\") (in (open \"hello.txt\"))) "
				   "(setq ret (read-line in)) (close in) ret)")->toString(), ==, "hello world");

	// append test
	compile(env, "(system \"rm -rf message.txt\")");
	ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-does-not-exist :create))) "
				   "(write-string \"hello \" f) (close f))")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-exists :append))) "
				   "(write-string \"world\" f) (close f))")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((ret \"\") (f (open \"message.txt\"))) "
				   "(setq ret (read-line f)) (close f) ret)")->toString(), ==, "hello world");

	// overwrite test
	ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-exists :overwrite))) "
				   "(write-string \"world\" f) (close f))")->isNil(), ==, true);
	ASSERT(compile(env, "(let ((ret \"\") (f (open \"message.txt\"))) "
				   "(setq ret (read-line f)) (close f) ret)")->toString(), ==, "world");

	// file-position
	compile(env, "(setq *f* (open \"message.txt\"))");
	ASSERT(*compile(env, "(file-position *f*)")->getInteger(), ==, 0);
	ASSERT(*compile(env, "(file-position *f* 2)")->getInteger(), ==, 2);
	ASSERT(compile(env, "(read-line *f*)")->toString(), ==, "rld");
	compile(env, "(close *f*)");
}

static void test_load() {
	Env env;
	native(env);

	compile(env, "(system \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *temp* 1)\" out) (close out))");
	compile(env, "(let ((in (open \"code.lsp\"))) (read in) (close in))");
	ASSERT(*compile(env, "*temp*")->getInteger(), ==, 1);

	//
	compile(env, "(system \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *a* \" out) (write-line \"1)\" out) (close out))");
	compile(env, "(let ((in (open \"code.lsp\"))) (read in) (close in))");
	ASSERT(*compile(env, "*a*")->getInteger(), ==, 1);

	//
	compile(env, "(system \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *b* \" out) (write-line \"1)\" out) (close out))");
	compile(env, "(load \"code.lsp\")");
	ASSERT(*compile(env, "*b*")->getInteger(), ==, 1);
}

static void test_error_handling() {
	Env env;
	native(env);

	ASSERT(compile(env, "(quote abc)")->getSymbol(), ==, "abc");

	try {
		compile(env, "(quote)");
		throw "This should not be thrown!";
	} catch (LispException e) {
		// OK
	}
}

static void test_block() {
	Env env;
	native(env);
	ASSERT(compile(env, "(block a (block nil (print 'a) (return-from nil)) (print 'b) (return-from a) (print 'c))")->isNil(), ==, true);
}

static void test_throw() {
	Env env;
	native(env);
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1))")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1) (print 'b))")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1) (print b))")->getInteger(), ==, 1);
	ASSERT(*compile(env, "(catch 'a (catch 'b (unwind-protect (throw 'a 1) (throw 'b 2))))")->getInteger(), ==, 2);

	string cmd = "(catch 'foo"
		"(format t \"The inner catch returns ~a.~%\""
		"(catch 'foo"
		"(unwind-protect (throw 'foo 'second-throw)"
		"(throw 'foo 'second-throw))))"
		"'outer-catch)";

	ASSERT(compile(env, cmd)->getSymbol(), ==, "outer-catch");
}

int main(int argc, char *args[]) {

	// Env::setDebug(true);
	// Var::setDebug(true);

	try {
		cout << " *** test_comment()" << endl;
		test_comment();
		cout << " *** test_subseq()" << endl;
		test_subseq();
		cout << " *** test_ref()" << endl;
		test_ref();
		cout << " *** test_quote()" << endl;
		test_quote();
		cout << " *** test_setf()" << endl;
		test_setf();
		cout << " *** test_func()" << endl;
		test_func();
		cout << " *** test_func_more()" << endl;
		test_func_more();
		cout << " *** test_type()" << endl;
		test_type();
		cout << " *** test_scope()" << endl;
		test_scope();
		cout << " *** test_logic()" << endl;
		test_logic();
		cout << " *** test_cond()" << endl;
		test_cond();
		cout << " *** test_loop()" << endl;
		test_loop();
		cout << " *** test_read()" << endl;
		test_read();
		cout << " *** test_string()" << endl;
		test_string();
		cout << " *** test_format()" << endl;
		test_format();
		cout << " *** test_list()" << endl;
		test_list();
		cout << " *** test_cons()" << endl;
		test_cons();
		cout << " *** test_arithmetic()" << endl;
		test_arithmetic();
		cout << " *** test_algorithm()" << endl;
		test_algorithm();
		cout << " *** test_file()" << endl;
		test_file();
		cout << " *** test_load()" << endl;
		test_load();
		cout << " *** test_error_handling()" << endl;
		test_error_handling();
		cout << " *** test_block()" << endl;
		test_block();
		cout << " *** test_throw()" << endl;
		test_throw();
	} catch (LispException & e) {
		cout << e.getMessage() << endl;
		exit(1);
	}
	
    return 0;
}

