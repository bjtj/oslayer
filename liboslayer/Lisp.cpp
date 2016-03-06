#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "FileReaderWriter.hpp"

#define DECL_NATIVE(NAME,CLS,CODE)								\
	class CLS : public Procedure {								\
	private:													\
	public:														\
	CLS(const string & name) : Procedure(name) {}				\
	virtual ~CLS() {}											\
	virtual Var proc(Var name, vector<Var> & args, Env & env)	\
		CODE;													\
	};															\
	env[NAME] = Var(UTIL::AutoRef<Procedure>(new CLS(NAME)));

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	// builtin
	static void builtin_algorithm(Env & env);
	static void builtin_list(Env & env);
	static void builtin_logic(Env & env);
	static void builtin_string(Env & env);
	static void builtin_artithmetic(Env & env);
	static void builtin_io(Env & env);
	static void builtin_file(Env & env);
	static void builtin_socket(Env & env);
	static void builtin_system(Env & env);
	static void builtin_date(Env & env);

	void testArgumentCount(vector<Var> & args, size_t expect) {
		if (args.size() < expect) {
			throw "Wrong argument count";
		}
	}

	static Var nil() {
		return Var("nil");
	}

	/**
	 *
	 */
	
	Arguments::Arguments() {}
	Arguments::Arguments(vector<Var> & proto) : proto(proto) {}
	Arguments::~Arguments() {}

	size_t Arguments::countPartArguments(vector<Var> & arr, size_t start) {
		size_t cnt = start;
		for (; cnt < arr.size(); cnt++) {
			if (Text::startsWith(arr[cnt].getSymbol(), "&")) {
				break;
			}
		}
		return cnt;
	}
	void Arguments::mapArguments(map<string, Var> & scope, vector<Var> & args) {

		size_t ec = countPartArguments(proto, 0);
		testArgumentCount(args, ec);

		size_t ai = 0;
		size_t i = 0;
		for (; i < ec; i++, ai++) {
			scope[proto[i].getSymbol()] = args[ai];
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i].getSymbol() == "&optional") {
			size_t offset = mapOptionals(scope, proto, ++i, args, ai);
			i += offset;
			ai += offset;
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i].getSymbol() == "&rest") {
			if (i + 1 >= proto.size()) {
				throw "Wrong function declaration";
			}
			scope[proto[i + 1].getSymbol()] = extractRest(args, ai);
		}

		keywords = extractKeywords(args);
	}
	size_t Arguments::mapOptionals(map<string, Var> & scope, vector<Var> & proto, size_t pstart, vector<Var> & args, size_t astart) {
		size_t i = pstart;
		size_t j = astart;
		for (; i < proto.size(); i++, j++) {

			if (proto[i].isSymbol() && Text::startsWith(proto[i].getSymbol(), "&")) {
				break;
			}

			string sym;
				
			if (proto[i].isSymbol()) {
				sym = proto[i].getSymbol();
				scope[sym] = nil();
			} else if (proto[i].isList()) {
				testArgumentCount(proto[i].getList(), 2);
				sym = proto[i].getList()[0].getSymbol();
				scope[sym] = proto[i].getList()[1];
			}

			if (j < args.size()) {
				scope[sym] = args[j];
			}
		}
		return i - pstart;
	}
	vector<Var> Arguments::extractRest(vector<Var> & args, size_t start) {
		vector<Var> rest;
		for (size_t i = start; i < args.size(); i++) {
			rest.push_back(args[i]);
		}
		return rest;
	}
	map<string, Var> Arguments::extractKeywords(vector<Var> & args) {
		map<string, Var> keywords;
		for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
			if (iter->isSymbol() && Text::startsWith(iter->getSymbol(), ":")) {
				string name = iter->getSymbol();
				Var val;
				if (iter + 1 != args.end()) {
					iter++;
					val = *iter;
				}
				keywords[name] = val;
			}
		}
		return keywords;
	}


	class Options {
	private:
		map<string, Var> options;
	public:
		Options(vector<Var> & args) {
			for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
				if (testVarSymbolStartsWith(*iter, ":")) {
					string name = iter->getSymbol();
					Var val;
					if (iter + 1 != args.end()) {
						iter++;
						val = *iter;
					}
					options[name] = val;
				}
			}
		}
		virtual ~Options() {}
		
		bool testVarSymbolStartsWith(Var & var, const std::string & start) {
			return (var.isSymbol() && Text::startsWith(var.getSymbol(), start));
		}
		bool has(const string & name) {
			return options.find(name) != options.end();
		}
		Var & operator[] (const string & name) {
			return options[name];
		}
	};

	static string format(Env & env, const string & fmt, vector<Var> & args, size_t offset) {
		string ret;
		size_t f = 0;
		size_t s = 0;
		vector<Var>::iterator iter = args.begin() + offset;
		while ((f = fmt.find("~", f)) != string::npos) {
			if (f - s > 0) {
				ret.append(fmt.substr(s, f - s));
			}

			if (fmt[f + 1] == '%') {
				ret.append("\n");
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'a') {
				ret.append(eval(*iter++, env).toString());
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'd') {
				string num = eval(*iter++, env).toString();
				ret.append(num);
				s = f = (f + 2);
			} else if (fmt[f + 1] == ':' && fmt[f + 2] == 'd') {
				string num = eval(*iter++, env).toString();
				size_t len = num.length();
				size_t cnt = (len - 1) / 3;
				if (cnt > 0) {
					size_t i = 3;
					for (size_t pos = (len - i); i < len; i += 3, pos = (len - i)) {
						num.insert(pos, ",");
					}
				}
				ret.append(num);
				s = f = (f + 3);
			} else {
				s = f = (f + 1);
			}
		}
		if (s < fmt.length()) {
			ret.append(fmt.substr(s));
		}
		return ret;
	}

	Var pathname(Var path) {
		if (path.isFile()) {
			return path;
		}
		File file(path.toString());
		return file;
	}

	string text(const string & txt) {
		return "\"" + txt + "\"";
	}
	
	string untext(const string & txt) {
		return txt.substr(1, txt.length() - 2);
	}

	vector<Var> listy(Var var) {
		if (var.isList()) {
			return var.getList();
		}
		vector<Var> ret;
		ret.push_back(var);
		return ret;
	}

	string replaceAll(string src, string match, string rep) {
		string ret = src;
		size_t f = 0;
		while ((f = ret.find(match, f)) != string::npos) {
			ret.replace(f, match.length(), rep);
			f += rep.length();
		}
		return ret;
	}

	vector<string> split(string target, string sep) {

		vector<string> vec;
		size_t s = 0;
		size_t f = 0;

		if (target.empty()) {
			return vec;
		}

		f = target.find(sep);

		while (f != string::npos) {
			if (f - s > 0) {
				vec.push_back(target.substr(s, f - s));
			}
			s = f + sep.length();
			f = target.find(sep, s);
		}
		if (s < target.size()) {
			vec.push_back(target.substr(s));
		}
		return vec;
	}

	bool isSpace(const char ch) {
		return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r';
	}

	vector<string> tokenize(string s) {
		vector<string> tokens;
		for (string::iterator iter = s.begin(); iter != s.end(); iter++) {
			if (*iter == '\"') {
				string str;
				iter++;
				if (iter == s.end()) {
					throw "unexpected end of string";
				}
				for (; *iter != '\"'; iter++) {
					char ch = *iter;
					if (iter == s.end()) {
						throw "unexpected end of string";
					}
					if (ch == '\\') {
						iter++;
						ch = *iter;
						switch (ch) {
						case 't':
							ch = '\t';
							break;
						case 'n':
							ch = '\n';
							break;
						case 'r':
							ch = '\r';
							break;
						default:
							break;
						}
					}
					str.append(1, ch);
				}
				tokens.push_back("\"" + str + "\"");
			} else if (*iter == '(' || *iter == ')') {
				tokens.push_back(string(1, *iter));
			} else if (!isSpace(*iter)) {
				string tok;
				for (; iter != s.end() && !isSpace(*iter) && *iter != '(' && *iter != ')'; iter++) {
					tok.append(1, *iter);
				}
				tokens.push_back(tok);
				iter--;
			}
		}
		return tokens;
	}

	Var read_from_tokens(vector<string>::iterator & iter, vector<string>::iterator & end) {

		if (iter == end) {
			throw "syntax error - unexpected EOF";
		}
		if (*iter == "(") {
			vector<Var> lst;
			iter++;
			for (;*iter != ")"; iter++) {
				Var var = read_from_tokens(iter, end);
				lst.push_back(var);
			}
			return Var(lst);
		} else if (*iter == ")") {
			throw "syntax error - unexpected )";
		} else {
			return Var(*iter);
		}
	}

	Var parse(const string & cmd) {
		vector<string> tok = tokenize(cmd);
		vector<string>::iterator iter = tok.begin();
		vector<string>::iterator end = tok.end();
		return read_from_tokens(iter, end);
	}

	Var & refeval(Var & var, Env & env) {
		if (var.isSymbol()) {
			return env[var.getSymbol()];
		} else if (!var.isList()) {
			throw "no reference";
		} else if (var.getList().empty()) {
			throw "cannot operate - no reference or value";
		} else {
			vector<Var> & lv = var.getList();
			string symbol = lv[0].getSymbol();
			if (symbol == "aref") {
				vector<Var> & lst = refeval(lv[1], env).getList();
				Integer idx = eval(lv[2], env).getInteger();
				return lst[(size_t)idx.getInteger()];
			}
		}
		throw "unknown error - reference operation failed";
	}

	Var eval(Var & var, Env & env) {
	
		if (var.isSymbol()) {
			return env[var.getSymbol()];
		} else if (!var.isList()) {
			return var;
		} else if (var.getList().empty()) {
			return nil();
		} else {
			vector<Var> & lv = var.getList();
			string symbol = lv[0].getSymbol();
			if (symbol == "quit") {
				env.quit(true);
			} else if (symbol == "symbol") {
				return lv[1].getSymbol();
			} else if (symbol == "lambda") {
				Var func(lv[1].getList(), lv[2].getList());
				return func;
			} else if (symbol == "defun") {
				env[lv[1].getSymbol()] = Var(lv[2].getList(), lv[3].getList());
				return lv[1].getSymbol();
			} else if (symbol == "setf") {
				Var val = eval(lv[2], env);
				if (lv[1].isList() && lv[1].getList().size() > 0 &&
					lv[1].getList()[0].isSymbol() && lv[1].getList()[0].getSymbol() == "subseq") {
					eval(lv[1], env) = val;
				} else {
					refeval(lv[1], env) = val;
				}
				return val;
			} else if (symbol == "setq") {
				Var val = eval(lv[2], env);
				env[lv[1].getSymbol()] = val;
				return val;
			} else if (symbol == "let") {
				Var ret;
				vector<Var> & lets = lv[1].getList();
				Env e(&env);
				for (vector<Var>::iterator iter = lets.begin(); iter != lets.end(); iter++) {
					vector<Var> decl = (*iter).getList();
					string symbol = decl[0].getSymbol();
					e.local()[symbol] = eval(decl[1], env);
				}
				for (vector<Var>::iterator iter = lv.begin() + 2; iter != lv.end(); iter++) {
					ret = eval(*iter, e);
				}
				return ret;
			} else if (symbol == "if") {
				Var val = eval(lv[1], env);
				if (!val.nil()) {
					return eval(lv[2], env);
				} else if (lv.size() > 3) {
					return eval(lv[3], env);
				}
			} else if (symbol == "when") {
				Var test = eval(lv[1], env);
				if (!test.nil()) {
					return eval(lv[2], env);
				}
			} else if (symbol == "unless") {
				Var test = eval(lv[1], env);
				if (test.nil()) {
					return eval(lv[2], env);
				}
				return nil();
			} else if (symbol == "cond") {
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					vector<Var> lst = iter->getList();
					if (!eval(lst[0], env).nil()) {
						return eval(lst[1], env);
					}
				}
				return nil();
			} else if (symbol == "progn") {
				Var ret;
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					ret = eval(*iter, env);
				}
				return ret;
			} else if (symbol == "while") {
				Var pre_test = lv[1];
				while (!eval(pre_test, env).nil()) {
					eval(lv[2], env);
				}
				return nil();
			} else if (symbol == "dolist") {
				Env e(&env);
				vector<Var> decl = lv[1].getList();
				string param = decl[0].getSymbol();
				vector<Var> lst = eval(decl[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
					e.local()[param] = eval(*iter, e);
					eval(lv[2], e);
				}
				return nil();
			} else if (symbol == "dotimes") {
				Env e(&env);
				vector<Var> steps = lv[1].getList();
				string sym = steps[0].getSymbol();
				Integer limit = eval(steps[1], env).getInteger();
				e.local()[sym] = Integer(0);
				for (; e[sym].getInteger() < limit; e[sym] = e[sym].getInteger() + 1) {
					eval(lv[2], e);
				}
				return nil();
			} else if (symbol == "list") {
				vector<Var> elts;
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					Var elt = eval(*iter, env);
					elts.push_back(elt);
				}
				return Var(elts);
			} else if (symbol == "cons") {
				Var cons = eval(lv[1], env);
				Var cell = eval(lv[2], env);
				Var var(cons, cell);
				return var;
			} else if (symbol == "car") {
				vector<Var> & lst = eval(lv[1], env).getList();
				if (lst.size() > 0) {
					return lst[0];
				}
				return nil();
			} else if (symbol == "cdr") {
				vector<Var> & lst = eval(lv[1], env).getList();
				if (lst.size() > 1) {
					vector<Var> rest = vector<Var>(lst.begin() + 1, lst.end());
					return Var(rest);
				}
				return nil();
			} else if (symbol == "nth") {
				int idx = (int)(*eval(lv[1], env).getInteger());
				vector<Var> & lst = eval(lv[2], env).getList();
				if (idx < lst.size()) {
					return lst[idx];
				}
				return nil();
			} else if (symbol == "nthcdr") {
				int idx = (int)(*eval(lv[1], env).getInteger());
				vector<Var> & lst = eval(lv[2], env).getList();
				if (idx < lst.size()) {
					vector<Var> rest(lst.begin() + idx, lst.end());
					return Var(rest);
				}
				return nil();
			} else if (symbol == "subseq") {
				// TODO: fix (subseq (list 1 2 3) 0 1) <-- not working
				vector<Var> & lst = refeval(lv[1], env).getList();
				Integer start = eval(lv[2], env).getInteger();
				Integer end = eval(lv[3], env).getInteger();
				vector<Var*> ret;
				vector<Var>::iterator iter = lst.begin() + start.getInteger();
				for (long long i = start.getInteger(); i < end.getInteger() && iter != lst.end(); i++, iter++) {
					ret.push_back(&(*iter));
				}
				return ret;
			} else if (symbol == "aref") { // bypass to refeval
				return refeval(var, env);
			} else {
				vector<Var> args(lv.begin() + 1, lv.end());
				return eval(lv[0], env).proc(lv[0], args, env);
			}
		}
	
		Var nil;
		return nil;
	}

	Var Var::proc(Var name, vector<Var> & args, Env & env) {

		if (!isFunction()) {
			throw "not function / " + name.toString();
		}

		if (!procedure.nil()) {
			return procedure->proc(name, args, env);
		}

		Env e(&env);
		vector<Var> proto = getParams().getList();
		Arguments binder(proto);
		binder.mapArguments(e.local(), args);
		Var body = getBody();
		return eval(body, e);
	}

	void native(Env & env) {
		builtin_algorithm(env);
		builtin_list(env);
		builtin_logic(env);
		builtin_string(env);
		builtin_artithmetic(env);
		builtin_io(env);
		builtin_file(env);
		builtin_socket(env);
		builtin_system(env);
		builtin_date(env);
	}

	void builtin_algorithm(Env & env) {
		DECL_NATIVE("map", Map, {
				Var sym = eval(args[0], env);
				Var func = eval(args[1], env);
				Var seq = eval(args[2], env);

				vector<Var> ret;

				vector<vector<Var> > lists;
				size_t size = 0;
				for (size_t i = 2; i < args.size(); i++) {
					vector<Var> lst = eval(args[i], env).getList();
					if (lst.size() > size) {
						size = lst.size();
					}
					lists.push_back(lst);
				}

				for (size_t i = 0; i < size; i++) {
					vector<Var> fargs;
					for (vector<vector<Var> >::iterator iter = lists.begin(); iter != lists.end(); iter++) {
						vector<Var> & lst = (*iter);
						fargs.push_back((i < lst.size() ? lst[i] : nil()));
					}
					ret.push_back(func.proc(fargs, env));
				}
				
				return ret;
			});
	}

	void builtin_list(Env & env) {
		DECL_NATIVE("append", Append, {
				vector<Var> ret;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					vector<Var> lst = eval(*iter, env).getList();
					ret.insert(ret.end(), lst.begin(), lst.end());
				}
				return ret;
			});
		
		DECL_NATIVE("remove", Remove, {
				Var val = eval(args[0], env);
				vector<Var> lst = eval(args[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end();) {
					if (val.toString() == iter->toString()) {
						iter = lst.erase(iter);
					} else {
						iter++;
					}
				}
				return lst;
			});

		DECL_NATIVE("remove-if", RemoveIf, {
				Var func = eval(args[0], env);
				vector<Var> lst = eval(args[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end();) {
					vector<Var> fargs;
					fargs.push_back(*iter);
					if (!func.proc(fargs, env).nil()) {
						iter = lst.erase(iter);
					} else {
						iter++;
					}
				}
				return lst;
			});
	}

	void builtin_logic(Env & env) {
		DECL_NATIVE("not", Not, {
				Var var = eval(args[0], env);
				return var.nil();
			});

		DECL_NATIVE("or", Or, {
				Var var;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (!var.nil()) {
						break;
					}
				}
				return var;
			});

		DECL_NATIVE("and", And, {
				Var var("t");
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (var.nil()) {
						break;
					}
				}
				return var;
			});
	}

	void builtin_string(Env & env) {

		DECL_NATIVE("string=", LiteralEqual, {
				string val = eval(args[0], env).toString();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).toString()) {
						return nil();
					}
				}
				return true;
			});

		DECL_NATIVE("string-prefix-p", StringPrefixP, {
				string str = eval(args[0], env).toString();
				string dst = eval(args[1], env).toString();
				return Text::startsWith(str, dst);
			});

		DECL_NATIVE("string-suffix-p", StringSuffixP, {
				string str = eval(args[0], env).toString();
				string dst = eval(args[1], env).toString();
				return Text::endsWith(str, dst);
			});

		DECL_NATIVE("string-length", StringLength, {
				return Integer((long long)eval(args[0], env).toString().length());
			});

		DECL_NATIVE("string-append", StringAppend, {
				string ret;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					ret.append(eval(*iter, env).toString());
				}
				return text(ret);
			});

		DECL_NATIVE("format", Format, {
				Var test = eval(args[0], env);
				string str = format(env, args[1].toString(), args, 2);
				if (!test.nil()) {
					fputs(str.c_str(), stdout);
					fputs("\n", stdout);
					Var nil;
					return nil;
				} else {
					return text(str);
				}
			});
		
		DECL_NATIVE("enough-namestring", EnoughNamestring, {
				string org = eval(args[0], env).toString();
				string prefix = eval(args[1], env).toString();
				if (Text::startsWith(org, prefix)) {
					return text(org.substr(prefix.length()));
				}
				return text(org);
			});
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE("=", ArithmeticEqual, {
				Integer val = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).getInteger()) {
						return nil();
					}
				}
				return true;
			});
		DECL_NATIVE("+", Plus, {
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum += eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("-", Minus, {
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum -= eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("*", Multitude, {
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum *= eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("/", Divide, {
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum /= eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("%", Rest, {
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum %= eval(*iter, env).getInteger();
				}
				return sum;
			});

		DECL_NATIVE(">", Greater, {
				Integer a = eval(args[0], env).getInteger();
				Integer b = eval(args[1], env).getInteger();
				return a > b;
			});

		DECL_NATIVE("<", Less, {
				Integer a = eval(args[0], env).getInteger();
				Integer b = eval(args[1], env).getInteger();
				return a < b;
			});

		DECL_NATIVE(">=", GreaterEq, {
				Integer a = eval(args[0], env).getInteger();
				Integer b = eval(args[1], env).getInteger();
				return a >= b;
			});

		DECL_NATIVE("<=", LessEq, {
				Integer a = eval(args[0], env).getInteger();
				Integer b = eval(args[1], env).getInteger();
				return a <= b;
			});
	}
	void builtin_io(Env & env) {

		// TODO: need file descriptor type
		env["*standard-output*"] = FileDescriptor(stdout);
		env["*standard-input*"] = FileDescriptor(stdin);
		
		DECL_NATIVE("read", Read, {
				FileDescriptor fd = eval(args[0], env).getFileDescriptor();
				if (fd.eof()) {
					return true;
				}
				string line = Text::trim(fd.read());
				if (!line.empty() && !Text::startsWith(line, ";")) {
					Var tokens = parse(line);
					return eval(tokens, env);
				}
				return nil();
			});
		DECL_NATIVE("read-line", ReadLine, {
				FileDescriptor fd = eval(args[0], env).getFileDescriptor();
				if (fd.eof()) {
					return true;
				}
				string line = fd.read();
				return text(line);
			});
		DECL_NATIVE("print", Print, {
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				fd.write("\n");
				return text(msg);
			});

		DECL_NATIVE("write-string", WriteString, {
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				return text(msg);
			});

		DECL_NATIVE("write-line", WriteLine, {
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				fd.write("\n");
				return text(msg);
			});
	}
	void builtin_file(Env & env) {
		DECL_NATIVE("pathname", Pathname, {
				return pathname(eval(args[0], env));
			});
		DECL_NATIVE("dir", Dir, {
				Var path = args.size() > 0 ? pathname(eval(args[0], env)) : "#p\".\"";
				vector<File> files = File::list(path.getFile().getPath());
				vector<Var> lst;
				for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
					lst.push_back(*iter);
				}
				return lst;
			});
		DECL_NATIVE("probe-file", ProbeFile, {
				File file = pathname(eval(args[0], env)).getFile();
				return file.exists();
			});
		DECL_NATIVE("dirp", Dirp, {
				File file = pathname(eval(args[0], env)).getFile();
				return file.isDirectory();
			});
		DECL_NATIVE("filep", Filep, {
				File file = pathname(eval(args[0], env)).getFile();
				return file.isFile();
			});
		DECL_NATIVE("pathname-name", PathnameName, {
				File file = pathname(eval(args[0], env)).getFile();
				return text(file.getName());
			});
		DECL_NATIVE("pathname-type", PathnameType, {
				File file = pathname(eval(args[0], env)).getFile();
				return text(file.getExtension());
			});
		DECL_NATIVE("directory-namestring", DirectoryNamestring, {
				File file = pathname(eval(args[0], env)).getFile();
				return text(file.getPathPart());
			});
		DECL_NATIVE("file-length", FileLength, {
				File file = pathname(eval(args[0], env)).getFile();
				return Integer(file.getSize());
			});
		DECL_NATIVE("open", Open, {
				File file = pathname(eval(args[0], env)).getFile();
				Options opts(args);
				const char * opt = "rb+";
				if (opts[":if-does-not-exist"].isSymbol() && opts[":if-does-not-exist"].getSymbol() == ":create") {
					opt = "wb+";
				}
				FILE * fp = fopen(file.getPath().c_str(), opt);
				if (!fp) {
					return nil(); // or throw exception
				}
				return FileDescriptor(fp);
			});
		DECL_NATIVE("close", Close, {
				eval(args[0], env).getFileDescriptor().close();
				return nil();
			});
	}
	void builtin_socket(Env & env) {
	}
	void builtin_system(Env & env) {
		DECL_NATIVE("system", System, {
				return Integer(system(eval(args[0], env).toString().c_str()));
			});
		DECL_NATIVE("load", Load, {
				File file = pathname(eval(args[0], env)).getFile();
				FileReader reader(file);
				string src = reader.dumpAsString();
				vector<string> lines = Text::split(src, "\n");
				for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
					string line = Text::trim(*iter);
					if (!line.empty() && !Text::startsWith(line, ";")) {
						Var tokens = parse(line);
						eval(tokens, env);
					}
				}
				return true;
			});
	}
	void builtin_date(Env & env) {
		DECL_NATIVE("now", Now, {
				char buffer[512];
				memset(buffer, 0, sizeof(buffer));
				Date date = Date::now();
				snprintf(buffer, sizeof(buffer), "%04d-%02d-%02d %02d:%02d:%02d.%d",
						 date.getYear(), date.getMonth(), date.getDay(),
						 date.getHour(), date.getMinute(), date.getSecond(),
						 date.getMillisecond());
				return string(buffer);
			});
	}

	BufferedCommandReader::BufferedCommandReader() {
	}
	BufferedCommandReader::~BufferedCommandReader() {
	}
	void BufferedCommandReader::clearCommands() {
		commands.clear();
	}
	void BufferedCommandReader::clearBuffer() {
		buffer = "";
	}
	size_t BufferedCommandReader::testComplete(const string & text) {
		size_t brace_count = 0;
		bool ignore = false;
		size_t i = text.find_first_not_of(" \t\r\n");
		if (i == string::npos) {	
			return 0;
		}

		if (text[i] != '(') {
			size_t e = text.find_first_of(" \t\r\n", i);
			if (e == string::npos) {
				return text.length();
			} else {
				return e;
			}
		}
		
		for (; i < text.size(); i++) {
			char ch = text[i];
			switch (ch) {
			case '(':
				brace_count++;
				break;
			case ')':
				if (brace_count == 0) {
					throw "unexpected end of command";
				}
				if (brace_count > 0) {
					brace_count--;
				}
				if (brace_count == 0) {
					return i + 1;
				}
				break;
			case '\\':
				if (ignore) {
					i++;
				}
				break;
			case '\"':
				ignore = !ignore;
				break;
			default:
				break;
			}
		}
		return 0;
	}
	size_t BufferedCommandReader::read(const string & text) {
		size_t c = 0;
		buffer.append(text);
		while ((c = testComplete(buffer)) > 0) {
			commands.push_back(buffer.substr(0, c));
			buffer = buffer.substr(c);
		}
		return commands.size();
	}
	size_t BufferedCommandReader::size() {
		return commands.size();
	}
	vector<string> & BufferedCommandReader::getCommands() {
		return commands;
	}
	string & BufferedCommandReader::operator[] (size_t idx) {
		return commands[idx];
	}

	string printVar(const Var & var) {
		if (var.isString()) {
			return var.getString();
		}
		return var.toString();
	}

	void repl(Env & env) {
		BufferedCommandReader reader;
		char line[1024] = {0,};
		cout << "> ";
		while (fgets(line, sizeof(line), stdin)) {
			if (reader.read(line) > 0) {
				vector<string> & commands = reader.getCommands();
				for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
					Var var = parse(*iter);
					cout << printVar(eval(var, env)) << endl;
				}
				reader.clearCommands();
				return;
			}
		}
	}
}
