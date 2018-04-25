#include <iostream>
#include <liboslayer/Lisp.hpp>
#include "utils.hpp"

using namespace std;
using namespace osl;
using namespace lisp;

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
	ASSERT(compile(env, "(format nil \"hello\") ; comment")->toPrintString(), ==, "hello");
	ASSERT(BufferedCommandReader::eliminateComment("(format nil ;comment\n"
											  "\"hello\")"), ==, "(format nil \n"
		   "\"hello\")");
	ASSERT(compile(env, "(format nil\n"
				   "\"hello\")")->toPrintString(), ==, "hello");
	ASSERT(compile(env, "(format nil ; comment \n"
				   "\"hello\")")->toPrintString(), ==, "hello");
}

static void test_raw_type() {

	// Boolean
	
	Boolean a;
	Boolean b;

	ASSERT(a == b, ==, true);
	ASSERT(a != b, ==, false);

	a = true;

	ASSERT(a == b, ==, false);
	ASSERT(a != b, ==, true);

	b = a;

	ASSERT(a == b, ==, true);
	ASSERT(a != b, ==, false);
}

static void test_subseq() {
	Env env;
	native(env);
	ASSERT(compile(env, "(subseq (list 1 2 3) 0 2)")->r_list().size(), ==, 2);
}

static void test_ref() {
	Env env;
	native(env);

	ASSERT(compile(env, "(boundp 'foo)")->isNil(), ==, true);
	compile(env, "(defvar foo 1)");
	ASSERT(compile(env, "(boundp 'foo)")->isNil(), ==, false);

	compile(env, "(defparameter *x* 1)");
	ASSERT(*compile(env, "*x*")->r_integer(), ==, 1);
	compile(env, "(defparameter *x* 2)");
	ASSERT(*compile(env, "*x*")->r_integer(), ==, 2);
	compile(env, "(defvar *x* 3)");
	ASSERT(*compile(env, "*x*")->r_integer(), ==, 2);
	compile(env, "(defvar *y*)");
	try {
		compile(env, "*y*");
	} catch (LispException e) {
		cout << "expected exception: " << e.toString() << endl;
	}
	compile(env, "(defvar *y* 1)");
	ASSERT(*compile(env, "*y*")->r_integer(), ==, 1);
	compile(env, "(defvar *y* 10)");
	ASSERT(*compile(env, "*y*")->r_integer(), ==, 1);

	ASSERT(*compile(env, "(setq a (car (list 1 2 3)))")->r_integer(), ==, 1);
	ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
	ASSERT(*compile(env, "(setq b (cdr (list 1 2 3)))")->r_list()[0]->r_integer(), ==, 2);
	ASSERT(*env.scope()->get_var(Symbol("b"))->r_list()[1]->r_integer(), ==, 3);

	compile(env, "(setq lst (list (list 1 2) (list 3 4)))");
	ASSERT(*compile(env, "(car (car lst))")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(car (cdr (car lst)))")->r_integer(), ==, 2);

	ASSERT(compile(env, "(car (cdr lst))")->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(car (cdr lst))")->r_list()[0]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(car (cdr lst))")->r_list()[1]->r_integer(), ==, 4);
	
	ASSERT(*compile(env, "(car (car (cdr lst)))")->r_integer(), ==, 3);
	ASSERT(*compile(env, "(car (cdr (car (cdr lst))))")->r_integer(), ==, 4);
}

static void test_scope() {
	Env env;
	native(env);

	UnsafeAutoRef<Scope> global_scope(new Scope);
	UnsafeAutoRef<Scope> local_scope(new Scope);
	UnsafeAutoRef<Scope> leaf_scope(new Scope);

	global_scope->put_var(Symbol("a"), env.alloc(new Var("A")));
	local_scope->put_var(Symbol("b"), env.alloc(new Var("B")));

	ASSERT(global_scope->get_var(Symbol("a")).nil(), ==, false);
	ASSERT(global_scope->get_var(Symbol("a"))->toPrintString(), ==, "A");
	ASSERT(local_scope->rsearch_var(Symbol("b")).nil(), ==, false);
	ASSERT(local_scope->get_var(Symbol("b"))->toPrintString(), ==, "B");
	ASSERT(local_scope->rsearch_var(Symbol("a")).nil(), ==, true);

	local_scope->parent() = global_scope;
	ASSERT(local_scope->rsearch_var(Symbol("a")).nil(), ==, false);
	ASSERT(local_scope->rsearch_var(Symbol("a"))->toPrintString(), ==, "A");

	local_scope->rput_var(Symbol("c"), env.alloc(new Var("C")));
	ASSERT(global_scope->get_var(Symbol("c")).nil(), ==, false);
	ASSERT(global_scope->get_var(Symbol("c"))->toPrintString(), ==, "C");
	try {
		ASSERT(local_scope->get_var(Symbol("c"))->isNil(), ==, true);
		throw "This should not be thrown!";
	} catch (LispException & e) {
		//
		cout << " -- expected exception : " << e.what() << endl;
	}
	ASSERT(local_scope->rsearch_var(Symbol("c")).nil(), ==, false);
	ASSERT(local_scope->rsearch_var(Symbol("c"))->toPrintString(), ==, "C");

	leaf_scope->parent() = local_scope;
	ASSERT(leaf_scope->rsearch_var(Symbol("a")).nil(), ==, false);
	ASSERT(leaf_scope->rsearch_var(Symbol("a"))->toPrintString(), ==, "A");
	ASSERT(leaf_scope->rsearch_var(Symbol("b")).nil(), ==, false);
	ASSERT(leaf_scope->rsearch_var(Symbol("b"))->toPrintString(), ==, "B");
	ASSERT(leaf_scope->rsearch_var(Symbol("c")).nil(), ==, false);
	ASSERT(leaf_scope->rsearch_var(Symbol("c"))->toPrintString(), ==, "C");
}

static void test_quote() {
	Env env;
	native(env);

	ASSERT(compile(env, "'(1 2 3)")->r_list().size(), ==, 3);
	ASSERT(*compile(env, "'(1 2 3)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "'(1 2 3)")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "'(1 2 3)")->r_list()[2]->r_integer(), ==, 3);

	ASSERT(compile(env, "`(1 2 3)")->r_list().size(), ==, 3);
	ASSERT(*compile(env, "`(1 2 3)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "`(1 2 3)")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "`(1 2 3)")->r_list()[2]->r_integer(), ==, 3);

	ASSERT(compile(env, "`(1 2 ,(+ 2 3))")->r_list().size(), ==, 3);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "`(1 2 ,(+ 2 3))")->r_list()[2]->r_integer(), ==, 5);

	ASSERT(*compile(env, "`(1 2 ,@(list 3 4))")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "`(1 2 ,@(list 3 4))")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "`(1 2 ,@(list 3 4))")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "`(1 2 ,@(list 3 4))")->r_list()[3]->r_integer(), ==, 4);
}

static void test_setf() {
	Env env;
	native(env);

	compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))");
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list().size(), ==, 2);

	compile(env, "(setq n 2)");
	ASSERT(*env.scope()->get_var(Symbol("n"))->r_integer(), ==, 2);
	compile(env, "(setf n 7)");
	ASSERT(*env.scope()->get_var(Symbol("n"))->r_integer(), ==, 7);

	compile(env, "(setq lst (list 1 2 3))");
	compile(env, "(setf (car lst) 7)");
	
	ASSERT(env.scope()->get_var(Symbol("lst"))->r_list().size(), ==, 3);
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[0]->r_integer(), ==, 7);
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[2]->r_integer(), ==, 3);

	compile(env, "(setf (subseq lst 0 1) (list 9))");
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[0]->r_integer(), ==, 9);
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*env.scope()->get_var(Symbol("lst"))->r_list()[2]->r_integer(), ==, 3);

	ASSERT(compile(env, "(setf (list 1 2) (list 4 5))")->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(setf (list 1 2) (list 4 5))")->r_list()[0]->r_integer(), ==, 4);
	ASSERT(*compile(env, "(setf (list 1 2) (list 4 5))")->r_list()[1]->r_integer(), ==, 5);
	ASSERT(*compile(env, "(car (setf (list 1 2) (list 4 5)))")->r_integer(), ==, 4);
	ASSERT(*compile(env, "(car (cdr (setf (list 1 2) (list 4 5))))")->r_integer(), ==, 5);

	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list().size(), ==, 2);
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list()[0]->getType(),
		   ==, Var::INTEGER);
	ASSERT(compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list()[1]->getType(),
		   ==, Var::INTEGER);
	ASSERT(*compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list()[0]->r_integer(),
		   ==, 4);
	ASSERT(*compile(env, "(setf (subseq (list 1 2 3) 0 2) (list 4 5))")->r_list()[1]->r_integer(),
		   ==, 5);
	
}

static void test_func() {

	{
		Env env;
		native(env);

		_VAR proto = parse(env, "(a b c)");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);

		env.clear();

		env = Env();
		native(env);
		proto = parse(env, "(a b c &optional x)");
		params = Parameters::read(env, env.scope(), proto);
		input = parse(env, "(1 2 3)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;
	
		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(env.scope()->get_var(Symbol("x"))->isNil(), ==, true);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x)");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3 4)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 4);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional (x 1))");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 1);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional (x 1))");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3 4)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 4);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x &rest y)");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3 4)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 4);
		ASSERT(env.scope()->get_var(Symbol("y"))->isNil(), ==, true);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(a b c &optional x &rest y)");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3 4 5 6 7)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("a"))->r_integer(), ==, 1);
		ASSERT(*env.scope()->get_var(Symbol("b"))->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("c"))->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 4);
		ASSERT(env.scope()->get_var(Symbol("y"))->r_list().size(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[0]->r_integer(), ==, 5);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[1]->r_integer(), ==, 6);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[2]->r_integer(), ==, 7);
	}

	{
		//
		Env env;
		native(env);
		_VAR proto = parse(env, "(&optional x &rest y)");
		Parameters params = Parameters::read(env, env.scope(), proto);
		_VAR input = parse(env, "(1 2 3 4 5 6 7)");

		cout << "proto: " << proto->toString() << endl;
		cout << "input: " << input->toString() << endl;

		params.bind(env, env.scope(), env.scope(), input->r_list());
		ASSERT(*env.scope()->get_var(Symbol("x"))->r_integer(), ==, 1);
		ASSERT(env.scope()->get_var(Symbol("y"))->r_list().size(), ==, 6);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[0]->r_integer(), ==, 2);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[1]->r_integer(), ==, 3);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[2]->r_integer(), ==, 4);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[3]->r_integer(), ==, 5);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[4]->r_integer(), ==, 6);
		ASSERT(*env.scope()->get_var(Symbol("y"))->r_list()[5]->r_integer(), ==, 7);
	}

	{
		//
		Env env;
		native(env);
		compile(env, "(defun hello (a b c) (+ a 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->r_integer(), ==, 1);
		compile(env, "(defun hello (a b c) (+ b 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->r_integer(), ==, 2);
		compile(env, "(defun hello (a b c) (+ c 0))");
		ASSERT(*compile(env, "(hello 1 2 3)")->r_integer(), ==, 3);
		compile(env, "(defun hello (a b c) (+ a b c))");
		ASSERT(*compile(env, "(hello 1 2 3)")->r_integer(), ==, 6);
		compile(env, "(defun hello (a b c &optional x) (if x (+ a b c) (+ a b)))");
		ASSERT(*compile(env, "(hello 1 2 3)")->r_integer(), ==, 3);
		ASSERT(*compile(env, "(hello 1 2 3 t)")->r_integer(), ==, 6);
		compile(env, "(defun hello (a &optional b &rest c) (list a b c))");
		ASSERT(compile(env, "(hello 1 2 3 4 5)")->r_list().size(), ==, 3);
	}

	{
		//
		Env env;
		native(env);
		compile(env, "(setq *str* \"\")");
		compile(env, "(defun wr (x) (setq *str* (string-append *str* x)))");
		compile(env, "(dolist (x (list 1 2 3)) (wr x))");
		ASSERT(env.scope()->get_var(Symbol("*str*"))->toPrintString(), ==, "123");

		compile(env, "(run-process \"touch .temp\")");
		compile(env, "(defun foo (x) (filep x))");
		ASSERT(compile(env, "(foo \".temp\")")->r_boolean().val(), ==, true);
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
		err = e.toString();
	}
	ASSERT(err.empty(), ==, false);
	ASSERT(compile(env, "(funcall (quote hello))")->toPrintString(), ==, "hello");
	ASSERT(compile(env, "(funcall (function hello))")->toPrintString(), ==, "hello");
}

static void test_func_params() {
	Env env;
	native(env);

	compile(env, "(defun foo (&optional x y &rest r) (list x y r))");
	ASSERT(compile(env, "(foo)")->r_list()[0]->isNil(), ==, true);
	ASSERT(compile(env, "(foo)")->r_list()[1]->isNil(), ==, true);
	ASSERT(compile(env, "(foo)")->r_list()[2]->isNil(), ==, true);

	ASSERT(*compile(env, "(foo 1 2)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(foo 1 2)")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(compile(env, "(foo 1 2)")->r_list()[2]->isNil(), ==, true);

	ASSERT(*compile(env, "(foo 1 2 3)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(foo 1 2 3)")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(foo 1 2 3)")->r_list()[2]->r_list()[0]->r_integer(), ==, 3);

	compile(env, "(defun fxx (&optional x y &rest r &key (a 0) b c) (list x y r a b c))");

	ASSERT(compile(env, "(fxx)")->r_list().size(), ==, 6);
	ASSERT(compile(env, "(fxx)")->r_list()[0]->isNil(), ==, true);
	ASSERT(compile(env, "(fxx)")->r_list()[1]->isNil(), ==, true);
	ASSERT(compile(env, "(fxx)")->r_list()[2]->isNil(), ==, true);
	ASSERT(*compile(env, "(fxx)")->r_list()[3]->r_integer(), ==, 0);
	ASSERT(compile(env, "(fxx)")->r_list()[4]->isNil(), ==, true);
	ASSERT(compile(env, "(fxx)")->r_list()[5]->isNil(), ==, true);

	ASSERT(compile(env, "(fxx 1 2 :b 2)")->r_list().size(), ==, 6);
	ASSERT(*compile(env, "(fxx 1 2 :b 2)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(fxx 1 2 :b 2)")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(compile(env, "(fxx 1 2 :b 2)")->r_list()[2]->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(fxx 1 2 :b 2)")->r_list()[3]->r_integer(), ==, 0);
	ASSERT(*compile(env, "(fxx 1 2 :b 2)")->r_list()[4]->r_integer(), ==, 2);
	ASSERT(compile(env, "(fxx 1 2 :b 2)")->r_list()[5]->isNil(), ==, true);
}

static void test_procedure() {
	Env env;
	native(env);

	class MyProc : public Procedure {
	public:
		MyProc() {}
		virtual ~MyProc() {}
		LISP_PROCEDURE_PROC(env, scope, name, args) {
			return env.alloc(new Var((int)args.size()));
		}
	};
	env.scope()->put_func(Symbol("my-proc"), env.alloc(new Var(new MyProc)));
	ASSERT(*compile(env, "(my-proc 1)")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(my-proc 1 2)")->r_integer(), ==, 2);
}

static void test_macro() {
	Env env;
	native(env);

	try {
		compile(env, "(defmacro mac1 (a b) `(+ ,a (* ,b 3)))");
	} catch (...) {
		// ignore
	}
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
	compile(env, "(run-process \"touch xxx\")");
	compile(env, "(setq *f* (open \"xxx\"))");
	ASSERT(compile(env, "(streamp *f*)")->isNil(), ==, false);
}

static void test_let() {
	Env env;
	native(env);

	ASSERT(compile(env, "(let ((ret \"\")) ret)")->toPrintString(), ==, "");
	ASSERT(compile(env, "(let ((ret \"\")) (setq ret (list 1 2 3)) (print ret) ret)")->getTypeString(), ==, "LIST");
}

static void test_cond() {
	Env env;
	native(env);

	ASSERT(*compile(env, "(if nil 1 0)")->r_integer(), ==, 0);
	ASSERT(*compile(env, "(if t 1 0)")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(when t 1)")->r_integer(), ==, 1);
	ASSERT(compile(env, "(when nil 1)")->isNil(), ==, true);
	ASSERT(compile(env, "(unless t 1)")->isNil(), ==, true);
	ASSERT(*compile(env, "(unless nil 1)")->r_integer(), ==, 1);
	compile(env, "(setq a 5)");
	ASSERT(*compile(env, "(cond ((= a 5) 1) (t \"default\"))")->r_integer(), ==, 1);
	ASSERT(compile(env, "(cond ((string= a \"hack\") \"foo\") (t \"default\"))")->toPrintString(), ==, "default");
	
	ASSERT(compile(env, "(string< \"a\" \"b\")")->isNil(), ==, false);
	ASSERT(compile(env, "(string< \"b\" \"a\")")->isNil(), ==, true);
	
	ASSERT(compile(env, "(string> \"a\" \"b\")")->isNil(), ==, true);
	ASSERT(compile(env, "(string> \"b\" \"a\")")->isNil(), ==, false);
	
	ASSERT(compile(env, "(string<= \"a\" \"b\")")->isNil(), ==, false);
	ASSERT(compile(env, "(string<= \"b\" \"a\")")->isNil(), ==, true);
	
	ASSERT(compile(env, "(string>= \"a\" \"b\")")->isNil(), ==, true);
	ASSERT(compile(env, "(string>= \"b\" \"a\")")->isNil(), ==, false);
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
				ASSERT(*ret->r_integer(), ==, 3);
			}
			reader.clearCommands();
		}
	}
}

static void test_string() {
	Env env;
	native(env);
	_VAR ret = compile(env, "(setq hello \"Hello World\")");

	ASSERT(ret->toPrintString(), ==, "Hello World");
	ASSERT(env.scope()->get_var(Symbol("hello"))->toPrintString(), ==, "Hello World");

	ret = compile(env, "(enough-namestring \"/www/html/foo/bar/baz.html\" \"/www/\")");
	ASSERT(ret->toPrintString(), ==, "html/foo/bar/baz.html");

	ASSERT(*compile(env, "1")->r_integer(), ==, 1);

	ASSERT(compile(env, "(string-prefix-p \".bashrc\" \".\")")->isNil(), ==, false);
	ASSERT(compile(env, "(string-prefix-p \"bashrc\" \".\")")->isNil(), ==, true);

	ASSERT(compile(env, "(string-append \"hello\" \" world\")")->toPrintString(), ==, "hello world");

	ASSERT(*compile(env, "(string-length \"hello world\")")->r_integer(), ==, strlen("hello world"));
}

static void test_format() {
	Env env;
	native(env);

	ASSERT(compile(env, "(format nil \"hello world\")")->toPrintString(), ==, "hello world");
	ASSERT(compile(env, "(format nil \"hello, ~a?\" \"friend\")")->toPrintString(), ==, "hello, friend?");

	string err;
	try {
		compile(env, "(format nil \"hello ~a\")");
	} catch (Exception & e) {
		err = e.toString();
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

	ASSERT(*compile(env, "(* 4 1.2)")->r_float(), ==, 4.8);
	ASSERT(compile(env, "(oddp 0)")->isNil(), ==, true);
	ASSERT(compile(env, "(evenp 0)")->isNil(), ==, false);
	ASSERT(compile(env, "(oddp 1)")->isNil(), ==, false);
	ASSERT(compile(env, "(evenp 1)")->isNil(), ==, true);
	ASSERT(compile(env, "(oddp 2)")->isNil(), ==, true);
	ASSERT(compile(env, "(evenp 2)")->isNil(), ==, false);

	try {
		compile(env, "(/ 2 0)");
		throw "this should not be thrown";
	} catch (DivisionByZeroLispException e) {
		cout << "expected exception : " << e.toString() << endl;
	}
}

static void test_list() {

	Env env;
	native(env);
	// append
	ASSERT(compile(env, "(append '(1 2 3) '(4 5 6))")->r_list().size(), ==, 6);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[3]->r_integer(), ==, 4);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[4]->r_integer(), ==, 5);
	ASSERT(*compile(env, "(append '(1 2 3) '(4 5 6))")->r_list()[5]->r_integer(), ==, 6);

	compile(env, "(defparameter *lst* '(1 2))");
	ASSERT(compile(env, "(append *lst* '(3 4))")->r_list().size(), ==, 4);
	ASSERT(*compile(env, "(append *lst* '(3 4))")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(append *lst* '(3 4))")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(append *lst* '(3 4))")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(append *lst* '(3 4))")->r_list()[3]->r_integer(), ==, 4);

	ASSERT(compile(env, "*lst*")->r_list().size(), ==, 2);
	ASSERT(*compile(env, "*lst*")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "*lst*")->r_list()[1]->r_integer(), ==, 2);

	// remove-if
	ASSERT(compile(env, "(remove-if (lambda (x) (= x 1)) (list 1 2 1 3))")->r_list().size(), ==, 2);
	ASSERT(compile(env, "(remove-if #'(lambda (x) (= x 1)) '(1 2 1 3))")->r_list().size(), ==, 2);
	ASSERT(compile(env, "(remove-if #'oddp '(1 2 4 1 3 4 5))")->r_list().size(), ==, 3);
	ASSERT(compile(env, "(remove-if-not #'evenp '(1 2 3 4 5 6 7 8 9))")->r_list().size(), ==, 4);
	ASSERT(compile(env, "(remove-if #'evenp '(1 2 3 4 5 6 7 8 9))")->r_list().size(), ==, 5);

	compile(env, "(defparameter *lst* '(1 1 2 3))");
	ASSERT(compile(env, "(remove-if (lambda (x) (= x 1)) *lst*)")->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(remove-if (lambda (x) (= x 1)) *lst*)")->r_list()[0]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(remove-if (lambda (x) (= x 1)) *lst*)")->r_list()[1]->r_integer(), ==, 3);
	ASSERT(compile(env, "(remove-if #'evenp *lst*)")->r_list().size(), ==, 3);
	ASSERT(*compile(env, "(remove-if #'evenp *lst*)")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(remove-if #'evenp *lst*)")->r_list()[1]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(remove-if #'evenp *lst*)")->r_list()[2]->r_integer(), ==, 3);

	// list access
	compile(env, "(setq *lst* (list 1 2 3))");
	ASSERT(*compile(env, "(car *lst*)")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(car (list 1 2 3))")->r_integer(), ==, 1);
	ASSERT((compile(env, "(cdr (list 1 2 3))"))->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(cdr (list 1 2 3))")->r_list()[0]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(cdr (list 1 2 3))")->r_list()[1]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(nth 0 (list 1 2 3))")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(nth 1 (list 1 2 3))")->r_integer(), ==, 2);
	ASSERT(*compile(env, "(nth 2 (list 1 2 3))")->r_integer(), ==, 3);
	ASSERT(compile(env, "(nth 10 (list 1 2 3))")->isNil(), ==, true);
	ASSERT(compile(env, "(nthcdr 0 (list 1 2 3))")->r_list().size(), ==, 3);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 0 (list 1 2 3))")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(compile(env, "(nthcdr 1 (list 1 2 3))")->r_list().size(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 1 (list 1 2 3))")->r_list()[0]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(nthcdr 1 (list 1 2 3))")->r_list()[1]->r_integer(), ==, 3);
	ASSERT(compile(env, "(nthcdr 2 (list 1 2 3))")->r_list().size(), ==, 1);
	ASSERT(*compile(env, "(nthcdr 2 (list 1 2 3))")->r_list()[0]->r_integer(), ==, 3);
	ASSERT(compile(env, "(nthcdr 10 (list 1 2 3))")->isNil(), ==, true);

	compile(env, "(setq *lst* (list (list \"name\" \"steve\") (list \"age\" \"23\")))");
	ASSERT((compile(env, "(car *lst*)"))->r_list().size(), ==, 2);
	ASSERT((compile(env, "(car (car *lst*))"))->toPrintString(), ==, "name");
	ASSERT((compile(env, "(car (cdr (car *lst*)))"))->toPrintString(), ==, "steve");
}

static void test_cons() {
	Env env;
	native(env);

	compile(env, "(cons 1 2)");
	ASSERT(compile(env, "(cons 1 2)")->toString(), ==, "(1 2)");
	ASSERT(compile(env, "(cons 1 (cons 2 3)))")->toString(), ==, "(1 2 3)");
	ASSERT(compile(env, "(cons 1 (cons 2 (cons 3 4))))")->toString(), ==, "(1 2 3 4)");

	ASSERT(compile(env, "(car (cons 1 2))")->getType(), ==, Var::INTEGER);
	ASSERT(*compile(env, "(car (cons 1 2))")->r_integer(), ==, 1);
	ASSERT(compile(env, "(cdr (cons 1 2))")->getType(), ==, Var::LIST);
	ASSERT(*compile(env, "(cdr (cons 1 2))")->r_list()[0]->r_integer(), ==, 2);
}

static void test_algorithm() {
	Env env;
	native(env);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->r_list()[0]->r_integer(),
		   ==, 2);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->r_list()[1]->r_integer(),
		   ==, 3);
	ASSERT(*compile(env, "(map (quote list) (lambda (x) (+ x 1)) (list 1 2 3))")->r_list()[2]->r_integer(),
		   ==, 4);
	ASSERT(compile(env, "(sort (list 1 2 3 4) (lambda (a b) (> a b)))")->r_list().size(), ==, 4);

	string lst = "1 2 3 4";
	string cmp = "(lambda (a b) (< a b))";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[3]->r_integer(), ==, 4);

	lst = "4 3 2 1";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[3]->r_integer(), ==, 4);

	lst = "2 4 1 3";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[0]->r_integer(), ==, 1);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[1]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[2]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[3]->r_integer(), ==, 4);

	lst = "2 4 1 3";
	cmp = "(lambda (a b) (> a b))";
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[0]->r_integer(), ==, 4);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[1]->r_integer(), ==, 3);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[2]->r_integer(), ==, 2);
	ASSERT(*compile(env, "(sort (list " + lst + ") " + cmp + ")")->r_list()[3]->r_integer(), ==, 1);
}

static void test_file() {
	{
		Env env;
		native(env);

		compile(env, "(run-process \"rm hello.txt\")");
		ASSERT(compile(env, "(open \"hello.txt\" :if-does-not-exist nil)")->isNil(), ==, true);
		ASSERT(!compile(env, "(open \"hello.txt\" :if-does-not-exist :create)")->isNil(), ==, true);
		ASSERT(compile(env, "(let ((out (open \"hello.txt\" :if-does-not-exist :create))) "
					   "(write-line \"hello world\" out) (close out))")->isNil(), ==, true);
		ASSERT(compile(env, "(filep \"hello.txt\")")->r_boolean().val(), ==, true);
		ASSERT(compile(env, "(let ((in (open \"hello.txt\"))) (read-line in))")->toPrintString(), ==, "hello world");
		ASSERT(compile(env, "(let ((out (open \"hello.txt\" :if-does-not-exist :create))) "
					   "(print out) (write-string \"hello world\" out) (close out))")->isNil(), ==, true);

		printf(" -- debug --\n");
	}
	
	{
		Env env;
		native(env);

		ASSERT(compile(env, "(let ((ret \"\") (in (open \"hello.txt\"))) "
					   "(setq ret (read-line in)) (close in) ret)")->toPrintString(), ==, "hello world");
	}

	{
		Env env;
		native(env);

		// append test
		compile(env, "(run-process \"rm -rf message.txt\")");
		ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-does-not-exist :create))) "
					   "(write-string \"hello \" f) (close f))")->isNil(), ==, true);
		ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-exists :append))) "
					   "(write-string \"world\" f) (close f))")->isNil(), ==, true);
		ASSERT(compile(env, "(let ((ret \"\") (f (open \"message.txt\"))) "
					   "(setq ret (read-line f)) (close f) ret)")->toPrintString(), ==, "hello world");
	}

	{
		Env env;
		native(env);

		// overwrite test
		ASSERT(compile(env, "(let ((f (open \"message.txt\" :if-exists :overwrite))) "
					   "(write-string \"world\" f) (close f))")->isNil(), ==, true);
		ASSERT(compile(env, "(let ((ret \"\") (f (open \"message.txt\"))) "
					   "(setq ret (read-line f)) (close f) ret)")->toPrintString(), ==, "world");
	}

	{
		Env env;
		native(env);

		// file-position
		compile(env, "(setq *f* (open \"message.txt\"))");
		ASSERT(*compile(env, "(file-position *f*)")->r_integer(), ==, 0);
		ASSERT(*compile(env, "(file-position *f* 2)")->r_integer(), ==, 2);
		ASSERT(compile(env, "(read-line *f*)")->toPrintString(), ==, "rld");
		compile(env, "(close *f*)");
	}
}

static void test_load() {
	Env env;
	native(env);

	compile(env, "(run-process \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *temp* 1)\" out) (close out))");
	compile(env, "(let ((in (open \"code.lsp\"))) (eval (read in)) (close in))");
	ASSERT(*compile(env, "*temp*")->r_integer(), ==, 1);

	//
	compile(env, "(run-process \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *a* \" out) (write-line \"1)\" out) (close out))");
	compile(env, "(let ((in (open \"code.lsp\"))) (eval (read in)) (close in))");
	ASSERT(*compile(env, "*a*")->r_integer(), ==, 1);

	//
	compile(env, "(run-process \"rm code.lsp\")");

	compile(env, "(let ((out (open \"code.lsp\" :if-does-not-exist :create))) (write-line \"(setq *b* \" out) (write-line \"1)\" out) (close out))");
	compile(env, "(load \"code.lsp\")");
	ASSERT(*compile(env, "*b*")->r_integer(), ==, 1);
}

static void test_error_handling() {
	Env env;
	native(env);

	ASSERT(compile(env, "(quote abc)")->toPrintString(), ==, "abc");

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
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1))")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1) (print 'b))")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(catch nil (print 'a) (throw nil 1) (print b))")->r_integer(), ==, 1);
	ASSERT(*compile(env, "(catch 'a (catch 'b (unwind-protect (throw 'a 1) (throw 'b 2))))")->r_integer(), ==, 2);

	string cmd = "(catch 'foo"
		"(format t \"The inner catch returns ~a.~%\""
		"(catch 'foo"
		"(unwind-protect (throw 'foo 'second-throw)"
		"(throw 'foo 'second-throw))))"
		"'outer-catch)";

	ASSERT(compile(env, cmd)->toPrintString(), ==, "outer-catch");
}

static void test_recursive() {
	Env env;
	native(env);
	compile(env, "(defun factorial (n) (if (<= n 1) n (* n (factorial (- n 1)))))");
	ASSERT(*compile(env, "(factorial 4)")->r_integer(), ==, 24);
	compile(env, "(defmacro fact (n) `(if (<= ,n 1) ,n (* ,n (fact (- ,n 1)))))");
	ASSERT(*compile(env, "(fact 4)")->r_integer(), ==, 24);
}

static void test_closure() {
	Env env;
	native(env);
	compile(env, "(defun foo (n) (lambda (i) (incf n i)))");
	compile(env, "(defparameter x (foo 1))");
	ASSERT(*compile(env, "(funcall x 1)")->r_integer(), ==, 2);
	ASSERT(*compile(env, "(funcall x 1)")->r_integer(), ==, 3);
	ASSERT(*compile(env, "(funcall x 1)")->r_integer(), ==, 4);
	ASSERT(*compile(env, "(funcall x 1)")->r_integer(), ==, 5);
}

int main(int argc, char *args[]) {

	// Env::setDebug(true);
	// Var::setDebug(true);

	try {
		cout << " *** test_comment()" << endl;
		test_comment();
		cout << " *** test_raw_type()" << endl;
		test_raw_type();
		cout << " *** test_subseq()" << endl;
		test_subseq();
		cout << " *** test_ref()" << endl;
		test_ref();
		cout << " *** test_scope()" << endl;
		test_scope();
		cout << " *** test_quote()" << endl;
		test_quote();
		cout << " *** test_setf()" << endl;
		test_setf();
		cout << " *** test_func()" << endl;
		test_func();
		cout << " *** test_func_more()" << endl;
		test_func_more();
		cout << " *** test_func_params()" << endl;
		test_func_params();
		cout << " *** test_procedure()" << endl;
		test_procedure();
		cout << " *** test_macro()" << endl;
		test_macro();
		cout << " *** test_type()" << endl;
		test_type();
		cout << " *** test_let()" << endl;
		test_let();
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
		cout << " *** test_recursive()" << endl;
		test_recursive();
		cout << " *** test_closure()" << endl;
		test_closure();
	} catch (LispException & e) {
		cout << e.toString() << endl;
		exit(1);
	}
	
    return 0;
}

