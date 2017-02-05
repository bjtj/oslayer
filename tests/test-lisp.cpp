#include <iostream>
#include <liboslayer/Lisp.hpp>
#include "utils.hpp"

using namespace std;
using namespace OS;
using namespace LISP;

#define _VAR Obj<Var> 

static void test_comment() {
	string text = "hello ; comment";
	ASSERT(BufferedCommandReader::trimComment(text), ==, "hello ");
	text = "(defun hello () ;sample hello program\n"
		"(print \"hello world\")) ; print greeting";
	ASSERT(BufferedCommandReader::trimComment(text), ==, "(defun hello () \n"
		   "(print \"hello world\")) ");
	text = "\" ; \"";
	ASSERT(BufferedCommandReader::trimComment(text), ==, "\" ; \"");

	Env env;
	native(env);
	ASSERT(compile("(format nil \"hello\") ; comment", env)->toString(), ==, "hello");
	ASSERT(BufferedCommandReader::trimComment("(format nil ;comment\n"
											  "\"hello\")"), ==, "(format nil \n"
		   "\"hello\")");
	ASSERT(compile("(format nil\n"
				   "\"hello\")", env)->toString(), ==, "hello");
	ASSERT(compile("(format nil ; comment \n"
				   "\"hello\")", env)->toString(), ==, "hello");
}

static void test_subseq() {
	Env env;
	native(env);
	ASSERT(compile("(subseq (list 1 2 3) 0 2)", env)->getList().size(), ==, 2);
}

static void test_ref() {
	Env env;
	native(env);

	ASSERT(*compile("(setq a (car (list 1 2 3)))", env)->getInteger(), ==, 1);
	ASSERT(*env["a"]->getInteger(), ==, 1);
	ASSERT(*compile("(setq b (cdr (list 1 2 3)))", env)->getList()[0]->getInteger(), ==, 2);
	ASSERT(*env["b"]->getList()[1]->getInteger(), ==, 3);

	compile("(setq lst (list (list 1 2) (list 3 4)))", env);
	ASSERT(*compile("(car (car lst))", env)->getInteger(), ==, 1);
	ASSERT(*compile("(car (cdr (car lst)))", env)->getInteger(), ==, 2);

	ASSERT(compile("(car (cdr lst))", env)->getList().size(), ==, 2);
	ASSERT(*compile("(car (cdr lst))", env)->getList()[0]->getInteger(), ==, 3);
	ASSERT(*compile("(car (cdr lst))", env)->getList()[1]->getInteger(), ==, 4);
	
	ASSERT(*compile("(car (car (cdr lst)))", env)->getInteger(), ==, 3);
	ASSERT(*compile("(car (cdr (car (cdr lst))))", env)->getInteger(), ==, 4);
}

static void test_setf() {
	Env env;
	native(env);

	compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env);
	ASSERT(compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList().size(), ==, 2);

	compile("(setq n 2)", env);
	ASSERT(*env["n"]->getInteger(), ==, 2);
	compile("(setf n 7)", env);
	ASSERT(*env["n"]->getInteger(), ==, 7);

	compile("(setq lst (list 1 2 3))", env);
	compile("(setf (car lst) 7)", env);
	
	ASSERT(env["lst"]->getList().size(), ==, 3);
	ASSERT(*env["lst"]->getList()[0]->getInteger(), ==, 7);
	ASSERT(*env["lst"]->getList()[1]->getInteger(), ==, 2);
	ASSERT(*env["lst"]->getList()[2]->getInteger(), ==, 3);

	compile("(setf (subseq lst 0 1) (list 9))", env);
	ASSERT(*env["lst"]->getList()[0]->getInteger(), ==, 9);
	ASSERT(*env["lst"]->getList()[1]->getInteger(), ==, 2);
	ASSERT(*env["lst"]->getList()[2]->getInteger(), ==, 3);

	ASSERT(compile("(setf (list 1 2) (list 4 5))", env)->getList().size(), ==, 2);
	ASSERT(*compile("(setf (list 1 2) (list 4 5))", env)->getList()[0]->getInteger(), ==, 4);
	ASSERT(*compile("(setf (list 1 2) (list 4 5))", env)->getList()[1]->getInteger(), ==, 5);
	ASSERT(*compile("(car (setf (list 1 2) (list 4 5)))", env)->getInteger(), ==, 4);
	ASSERT(*compile("(car (cdr (setf (list 1 2) (list 4 5))))", env)->getInteger(), ==, 5);

	ASSERT(compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList().size(), ==, 2);
	ASSERT(compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList()[0]->getType(),
		   ==, Var::INTEGER);
	ASSERT(compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList()[1]->getType(),
		   ==, Var::INTEGER);
	ASSERT(*compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList()[0]->getInteger(),
		   ==, 4);
	ASSERT(*compile("(setf (subseq (list 1 2 3) 0 2) (list 4 5))", env)->getList()[1]->getInteger(),
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
		compile("(defun hello (a b c) (+ a 0))", env);
		ASSERT(*compile("(hello 1 2 3)", env)->getInteger(), ==, 1);
		compile("(defun hello (a b c) (+ b 0))", env);
		ASSERT(*compile("(hello 1 2 3)", env)->getInteger(), ==, 2);
		compile("(defun hello (a b c) (+ c 0))", env);
		ASSERT(*compile("(hello 1 2 3)", env)->getInteger(), ==, 3);
		compile("(defun hello (a b c) (+ a b c))", env);
		ASSERT(*compile("(hello 1 2 3)", env)->getInteger(), ==, 6);
		compile("(defun hello (a b c &optional x) (if x (+ a b c) (+ a b)))", env);
		ASSERT(*compile("(hello 1 2 3)", env)->getInteger(), ==, 3);
		ASSERT(*compile("(hello 1 2 3 t)", env)->getInteger(), ==, 6);
		compile("(defun hello (a &optional b &rest c) (list a b c))", env);
		ASSERT(compile("(hello 1 2 3 4 5)", env)->getList().size(), ==, 3);
	}

	{
		//
		Env env;
		native(env);
		compile("(setq str \"\")", env);
		compile("(defun wr (x) (setq str x))", env);
		compile("(dolist (x (list 1 2 3)) (wr x))", env);
	}
}

static void test_func_more() {
	Env env;
	native(env);

	compile("(defun hello () (print \"hello\"))", env);

	string err;
	try {
		compile("(funcall hello)", env);
	} catch (LispException & e) {
		err = e.getMessage();
	}
	ASSERT(err.empty(), ==, false);
	ASSERT(compile("(funcall (quote hello))", env)->toString(), ==, "hello");
	ASSERT(compile("(funcall (function hello))", env)->toString(), ==, "hello");
	
	// compile("(defun adder (x) (function (lambda (y) (+ x y))))", env);
	// compile("(setq add3 (adder 3))", env);
	// ASSERT(*compile("(funcall add3 5)", env)->getInteger(), ==, 8);
	
}

static void test_logic() {
	Env env;
	native(env);
	ASSERT(compile("(not t)", env)->isNil(), ==, true);
	ASSERT(compile("(not nil)", env)->isNil(), ==, false);
	ASSERT(compile("(or 1 1)", env)->isNil(), ==, false);
	ASSERT(compile("(and t nil)", env)->isNil(), ==, true);
	ASSERT(compile("(or t nil)", env)->isNil(), ==, false);
	ASSERT(compile("(or (and t nil) t))", env)->isNil(), ==, false);
	ASSERT(compile("(and (or nil t) nil)", env)->isNil(), ==, true);
}

static void test_type() {

	Env env;
	native(env);
	
	Var var(false);
	ASSERT(var.isNil(), ==, true);

	ASSERT(compile("(symbolp (quote x))", env)->isNil(), ==, false);
	ASSERT(compile("(listp (list 1 2 3))", env)->isNil(), ==, false);
	ASSERT(compile("(booleanp t)", env)->isNil(), ==, false);
	ASSERT(compile("(integerp 1)", env)->isNil(), ==, false);
	ASSERT(compile("(floatp 1.0)", env)->isNil(), ==, false);
	ASSERT(compile("(stringp \"hello\")", env)->isNil(), ==, false);
	compile("(system \"touch xxx\")", env);
	compile("(setq *f* (open \"xxx\"))", env);
	ASSERT(compile("(streamp *f*)", env)->isNil(), ==, false);
	// ASSERT(compile("(pathnamep (car (dir \".\")))", env)->isNil(), ==, false);
}

static void test_scope() {
	Env env;
	native(env);

	ASSERT(compile("(let ((ret \"\")) ret)", env)->toString(), ==, "");
	ASSERT(compile("(let ((ret \"\")) (setq ret (list 1 2 3)) (print ret) ret)", env)->getTypeString(), ==, "LIST");
}

static void test_cond() {
	Env env;
	native(env);

	ASSERT(*compile("(if nil 1 0)", env)->getInteger(), ==, 0);
	ASSERT(*compile("(if t 1 0)", env)->getInteger(), ==, 1);
	ASSERT(*compile("(when t 1)", env)->getInteger(), ==, 1);
	ASSERT(compile("(when nil 1)", env)->isNil(), ==, true);
	ASSERT(compile("(unless t 1)", env)->isNil(), ==, true);
	ASSERT(*compile("(unless nil 1)", env)->getInteger(), ==, 1);
	compile("(setq a 5)", env);
	ASSERT(*compile("(cond ((= a 5) 1) (t \"default\"))", env)->getInteger(), ==, 1);
	ASSERT(compile("(cond ((string= a \"hack\") \"foo\") (t \"default\"))", env)->toString(), ==, "default");
}

static void test_loop() {
	Env env;
	native(env);

	compile("(setq num 0)", env);
	ASSERT(compile("(while (< num 4) (progn (print num) (setq num (+ num 1)))))", env)->isNil(), ==, true);
	ASSERT(compile("(dolist (x (list 1 2 3)) (print x))", env)->isNil(), ==, true);
	ASSERT(compile("(dotimes (i 10) (print i))", env)->isNil(), ==, true);

	compile("(setq *lst* (list (list 1 2) (list 3 4)))", env);
	compile("(dolist (x *lst*) (print x))", env);
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
				_VAR ret = compile(*cmd, env);
				ASSERT(*ret->getInteger(), ==, 3);
			}
			reader.clearCommands();
		}
	}
}

static void test_string() {
	Env env;
	native(env);
	_VAR ret = compile("(setq hello \"Hello World\")", env);

	ASSERT(ret->toString(), ==, "Hello World");
	ASSERT(env["hello"]->toString(), ==, "Hello World");

	ret = compile("(enough-namestring \"/www/html/foo/bar/baz.html\" \"/www/\")", env);
	ASSERT(ret->toString(), ==, "html/foo/bar/baz.html");

	ASSERT(*compile("1", env)->getInteger(), ==, 1);

	ASSERT(compile("(string-prefix-p \".bashrc\" \".\")", env)->isNil(), ==, false);
	ASSERT(compile("(string-prefix-p \"bashrc\" \".\")", env)->isNil(), ==, true);

	ASSERT(compile("(string-append \"hello\" \" world\")", env)->toString(), ==, "hello world");

	ASSERT(*compile("(string-length \"hello world\")", env)->getInteger(), ==, strlen("hello world"));
}

static void test_format() {
	Env env;
	native(env);

	ASSERT(compile("(format nil \"hello world\")", env)->toString(), ==, "hello world");
	ASSERT(compile("(format nil \"hello, ~a?\" \"friend\")", env)->toString(), ==, "hello, friend?");

	string err;
	try {
		compile("(format nil \"hello ~a\")", env);
	} catch (Exception & e) {
		err = e.getMessage();
	}
	ASSERT(err, ==, "out of bound");
}

static void test_arithmetic() {
	Env env;
	native(env);
	ASSERT(!compile("(= 1 1)", env)->isNil(), ==, true);
	ASSERT(!compile("(= 2 1)", env)->isNil(), ==, false);
	ASSERT(!compile("(= 1 2)", env)->isNil(), ==, false);
	ASSERT(!compile("(< 1 4)", env)->isNil(), ==, true);
	ASSERT(!compile("(> 1 4)", env)->isNil(), ==, false);
	ASSERT(!compile("(<= 1 1)", env)->isNil(), ==, true);
	ASSERT(!compile("(<= 1 4)", env)->isNil(), ==, true);
	ASSERT(!compile("(<= 4 1)", env)->isNil(), ==, false);
	ASSERT(!compile("(>= 1 1)", env)->isNil(), ==, true);
	ASSERT(!compile("(>= 1 4)", env)->isNil(), ==, false);
	ASSERT(!compile("(>= 4 1)", env)->isNil(), ==, true);

	ASSERT(!compile("(= 1 1.0)", env)->isNil(), ==, true);
	ASSERT(!compile("(= 2 1.0)", env)->isNil(), ==, false);
	ASSERT(!compile("(= 1 2.0)", env)->isNil(), ==, false);
	ASSERT(!compile("(< 1 4.0)", env)->isNil(), ==, true);
	ASSERT(!compile("(> 1 4.0)", env)->isNil(), ==, false);
	ASSERT(!compile("(<= 1 1.0)", env)->isNil(), ==, true);
	ASSERT(!compile("(<= 1 4.0)", env)->isNil(), ==, true);
	ASSERT(!compile("(<= 4 1.0)", env)->isNil(), ==, false);
	ASSERT(!compile("(>= 1 1.0)", env)->isNil(), ==, true);
	ASSERT(!compile("(>= 1 4.0)", env)->isNil(), ==, false);
	ASSERT(!compile("(>= 4 1.0)", env)->isNil(), ==, true);

	ASSERT(*compile("(* 4 1.2)", env)->getFloat(), ==, 4.8f);
}

static void test_list() {

	Env env;
	native(env);
	ASSERT(compile("(remove-if (lambda (x) (= x 1)) (list 1 2 1 3))", env)->getList().size(), ==, 2);
	compile("(setq *lst* (list 1 2 3))", env);
	ASSERT(*compile("(car *lst*)", env)->getInteger(), ==, 1);
	ASSERT(*compile("(car (list 1 2 3))", env)->getInteger(), ==, 1);
	ASSERT((compile("(cdr (list 1 2 3))", env))->getList().size(), ==, 2);
	ASSERT(*compile("(cdr (list 1 2 3))", env)->getList()[0]->getInteger(), ==, 2);
	ASSERT(*compile("(cdr (list 1 2 3))", env)->getList()[1]->getInteger(), ==, 3);
	ASSERT(*compile("(nth 0 (list 1 2 3))", env)->getInteger(), ==, 1);
	ASSERT(*compile("(nth 1 (list 1 2 3))", env)->getInteger(), ==, 2);
	ASSERT(*compile("(nth 2 (list 1 2 3))", env)->getInteger(), ==, 3);
	ASSERT(compile("(nth 10 (list 1 2 3))", env)->isNil(), ==, true);
	ASSERT(compile("(nthcdr 0 (list 1 2 3))", env)->getList().size(), ==, 3);
	ASSERT(*compile("(nthcdr 0 (list 1 2 3))", env)->getList()[0]->getInteger(), ==, 1);
	ASSERT(*compile("(nthcdr 0 (list 1 2 3))", env)->getList()[1]->getInteger(), ==, 2);
	ASSERT(*compile("(nthcdr 0 (list 1 2 3))", env)->getList()[2]->getInteger(), ==, 3);
	ASSERT(compile("(nthcdr 1 (list 1 2 3))", env)->getList().size(), ==, 2);
	ASSERT(*compile("(nthcdr 1 (list 1 2 3))", env)->getList()[0]->getInteger(), ==, 2);
	ASSERT(*compile("(nthcdr 1 (list 1 2 3))", env)->getList()[1]->getInteger(), ==, 3);
	ASSERT(compile("(nthcdr 2 (list 1 2 3))", env)->getList().size(), ==, 1);
	ASSERT(*compile("(nthcdr 2 (list 1 2 3))", env)->getList()[0]->getInteger(), ==, 3);
	ASSERT(compile("(nthcdr 10 (list 1 2 3))", env)->isNil(), ==, true);

	compile("(setq *lst* (list (list \"name\" \"steve\") (list \"age\" \"23\")))", env);
	ASSERT((compile("(car *lst*)", env))->getList().size(), ==, 2);
	ASSERT((compile("(car (car *lst*))", env))->toString(), ==, "name");
	ASSERT((compile("(car (cdr (car *lst*)))", env))->toString(), ==, "steve");
}

static void test_cons() {
	Env env;
	native(env);

	compile("(cons 1 2)", env);
	ASSERT(compile("(cons 1 2)", env)->toString(), ==, "(1 2)");
	ASSERT(compile("(cons 1 (cons 2 3)))", env)->toString(), ==, "(1 2 3)");
	ASSERT(compile("(cons 1 (cons 2 (cons 3 4))))", env)->toString(), ==, "(1 2 3 4)");

	ASSERT(compile("(car (cons 1 2))", env)->getType(), ==, Var::INTEGER);
	ASSERT(*compile("(car (cons 1 2))", env)->getInteger(), ==, 1);
	ASSERT(compile("(cdr (cons 1 2))", env)->getType(), ==, Var::LIST);
	ASSERT(*compile("(cdr (cons 1 2))", env)->getList()[0]->getInteger(), ==, 2);
}

static void test_algorithm() {
	Env env;
	native(env);
	ASSERT(*compile("(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))", env)->getList()[0]->getInteger(),
		   ==, 2);
	ASSERT(*compile("(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))", env)->getList()[1]->getInteger(),
		   ==, 3);
	ASSERT(*compile("(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))", env)->getList()[2]->getInteger(),
		   ==, 4);
	ASSERT(compile("(sort (list 1 2 3 4) (lambda (a b) (> a b)))", env)->getList().size(), ==, 4);

	string lst = "1 2 3 4";
	string cmp = "(lambda (a b) (> a b))";
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[3]->getInteger(),
		   ==, 4);

	lst = "4 3 2 1";
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[3]->getInteger(),
		   ==, 4);

	lst = "2 4 1 3";
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[0]->getInteger(),
		   ==, 1);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[1]->getInteger(),
		   ==, 2);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[2]->getInteger(),
		   ==, 3);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[3]->getInteger(),
		   ==, 4);

	lst = "2 4 1 3";
	cmp = "(lambda (a b) (< a b))";
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[0]->getInteger(),
		   ==, 4);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[1]->getInteger(),
		   ==, 3);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[2]->getInteger(),
		   ==, 2);
	ASSERT(*compile("(sort (list " + lst + ") " + cmp + ")", env)->getList()[3]->getInteger(),
		   ==, 1);
}

static void test_file() {
	Env env;
	native(env);

	compile("(system \"rm hello.txt\")", env);
	//ASSERT(compile("(open \"hello.txt\")", env)->isNil(), ==, true);
	ASSERT(compile("(open \"hello.txt\" :if-does-not-exist nil)", env)->isNil(), ==, true);
	ASSERT(!compile("(open \"hello.txt\" :if-does-not-exist :create)", env)->isNil(), ==, true);
	ASSERT(compile("(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-line \"hello world\" out) (close out))", env)->isNil(), ==, true);
	ASSERT(compile("(let ((in (open \"hello.txt\"))) (read-line in))", env)->toString(), ==, "hello world");
	ASSERT(compile("(let ((out(open \"hello.txt\" :if-does-not-exist :create))) "
				   "(write-string \"hello world\" out) (close out))", env)->isNil(), ==, true);
	ASSERT(compile("(let ((ret \"\") (in (open \"hello.txt\"))) "
				   "(setq ret (read-line in)) (close in) ret)", env)->toString(), ==, "hello world");

	// append test
	compile("(system \"rm -rf message.txt\")", env);
	ASSERT(compile("(let ((f (open \"message.txt\" :if-does-not-exist :create))) "
				   "(write-string \"hello \" f) (close f))", env)->isNil(), ==, true);
	ASSERT(compile("(let ((f (open \"message.txt\" :if-exists :append))) "
				   "(write-string \"world\" f) (close f))", env)->isNil(), ==, true);
	ASSERT(compile("(let ((ret \"\") (f (open \"message.txt\"))) "
				   "(setq ret (read-line f)) (close f) ret)", env)->toString(), ==, "hello world");

	// overwrite test
	ASSERT(compile("(let ((f (open \"message.txt\" :if-exists :overwrite))) "
				   "(write-string \"world\" f) (close f))", env)->isNil(), ==, true);
	ASSERT(compile("(let ((ret \"\") (f (open \"message.txt\"))) "
				   "(setq ret (read-line f)) (close f) ret)", env)->toString(), ==, "world");

	// file-position
	compile("(setq *f* (open \"message.txt\"))", env);
	ASSERT(*compile("(file-position *f*)", env)->getInteger(), ==, 0);
	ASSERT(*compile("(file-position *f* 2)", env)->getInteger(), ==, 2);
	ASSERT(compile("(read-line *f*)", env)->toString(), ==, "rld");
	compile("(close *f*)", env);
}

static void test_load() {
	Env env;
	native(env);

	compile("(system \"rm code.lsp\")", env);

	compile("(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *temp* 1)\" out) (close out))", env);
	compile("(let ((in (open \"code.lsp\"))) (read in) (close in))", env);
	ASSERT(*compile("*temp*", env)->getInteger(), ==, 1);

	//
	compile("(system \"rm code.lsp\")", env);

	compile("(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *a* \" out) (write-line \"1)\" out) (close out))", env);
	compile("(let ((in (open \"code.lsp\"))) (read in) (close in))", env);
	ASSERT(*compile("*a*", env)->getInteger(), ==, 1);

	//
	compile("(system \"rm code.lsp\")", env);

	compile("(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *b* \" out) (write-line \"1)\" out) (close out))", env);
	compile("(load \"code.lsp\")", env);
	ASSERT(*compile("*b*", env)->getInteger(), ==, 1);
}

static void test_error_handling() {
	Env env;
	native(env);

	ASSERT(compile("(quote abc)", env)->getSymbol(), ==, "abc");

	try {
		compile("(quote)", env);
	} catch (LispException e) {
		ASSERT(e.getMessage(), ==, "Wrong argument count");
	}
}

int main(int argc, char *args[]) {

	Env::setDebug(true);
	Var::setDebug(true);

	try {
		cout << " *** test_comment()" << endl;
		test_comment();
		cout << " *** test_subseq()" << endl;
		test_subseq();
		cout << " *** test_ref()" << endl;
		test_ref();
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
	} catch (LispException & e) {
		cout << e.getMessage() << endl;
		exit(1);
	}
	
    return 0;
}

