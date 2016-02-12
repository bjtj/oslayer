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

	static string untext(const string & txt);

	// builtin
	static void builtin_list(Env & env);
	static void builtin_logic(Env & env);
	static void builtin_string(Env & env);
	static void builtin_artithmetic(Env & env);
	static void builtin_io(Env & env);
	static void builtin_file(Env & env);
	static void builtin_socket(Env & env);
	static void builtin_system(Env & env);
	static void builtin_date(Env & env);

	string printVar(const Var & var) {
		if (var.isString()) {
			return untext(var.toString());
		}
		return var.toString();
	}

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
				ret.append(printVar(eval(*iter++, env)));
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
			Var nil;
			return nil;
		} else {
			vector<Var> & lv = var.getList();
			string symbol = lv[0].getSymbol();
			if (symbol == "quit") {
				env.quit(true);
			} else if (symbol == "lambda") {
				Var func(lv[1].getList(), lv[2].getList());
				return func;
			} else if (symbol == "defun") {
				env[lv[1].getSymbol()] = Var(lv[2].getList(), lv[3].getList());
				return lv[1].getSymbol();
			} else if (symbol == "setf") {
				Var val = eval(lv[2], env);
				refeval(lv[1], env) = val;
				return val;
			} else if (symbol == "setq") {
				Var val = eval(lv[2], env);
				env[lv[1].getSymbol()] = val;
				return val;
			} else if (symbol == "if") {
				Var val = eval(lv[1], env);
				if (!val.nil() && val.getBoolean()) {
					return eval(lv[2], env);
				} else if (lv.size() > 3) {
					return eval(lv[3], env);
				}
			} else if (symbol == "progn") {
				Var ret;
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					ret = eval(*iter, env);
				}
				return ret;
			} else if (symbol == "dolist") {
				Env e(&env);
				vector<Var> decl = lv[1].getList();
				string param = decl[0].getSymbol();
				vector<Var> lst = eval(decl[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
					e.local(param) = eval(*iter, e);
					eval(lv[2], e);
				}
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
			} else if (symbol == "aref") {
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

		if (!procedure.empty()) {
			return procedure->proc(name, args, env);
		}

		Env e(&env);
		vector<Var> params = getParams().getList();
		vector<Var>::iterator iparams = params.begin();
		vector<Var>::iterator iargs = args.begin();
		for (; iparams != params.end() && iargs != args.end(); iparams++, iargs++) {
			e.local(iparams->getSymbol()) = eval(*iargs, env);
		}
		Var var = getBody();
		return eval(var, e);
	}

	void native(Env & env) {
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
	}

	void builtin_logic(Env & env) {
		DECL_NATIVE("not", Not, {
				Var var = eval(args[0], env);
				return (var.nil() ? true : !var.getBoolean());
			});

		DECL_NATIVE("or", Or, {
				Var var;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (!var.nil() && (!var.isBoolean() || var.getBoolean() == true)) {
						break;
					}
				}
				return var;
			});

		DECL_NATIVE("and", And, {
				Var var("t");
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (var.nil() || (var.isBoolean() && var.getBoolean() == false)) {
						break;
					}
				}
				return var;
			});
	}

	void builtin_string(Env & env) {

		DECL_NATIVE("eq", LiteralEqual, {
				string val = eval(args[0], env).toString();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).toString()) {
						return false;
					}
				}
				return true;
			});

		DECL_NATIVE("string-append", StringAppend, {
				string sym = args[0].getSymbol();
				Var val = eval(args[0], env);
				string str = val.nil() ? "" : val.toString();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					str += eval(*iter, env).toString();
				}
				env[sym] = str;
				return env[sym];
			});

		DECL_NATIVE("format", Format, {
				Var test = eval(args[0], env);
				string str = format(env, printVar(args[1]), args, 2);
				if (!test.nil()) {
					fputs(str.c_str(), stdout);
					fputs("\n", stdout);
					Var nil;
					return nil;
				} else {
					return text(str);
				}
			});
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE("=", ArithmeticEqual, {
				Integer val = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).getInteger()) {
						return false;
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
		DECL_NATIVE("read", Read, {
				char buffer[1024];
				memset(buffer, 0, sizeof(buffer));
				if (fgets(buffer, sizeof(buffer), stdin)) {
					buffer[strlen(buffer) - 1] = '\0';
					return Var(string(buffer));
				}
				return "nil";
			});
		DECL_NATIVE("print", Print, {
				string msg = eval(args[0], env).toString();
				fputs(msg.c_str(), stdout);
				fputs("\n", stdout);
				return msg;
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
					if (!line.empty() && Text::startsWith(line, ";")) {
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

	void repl(Env & env) {
		char line[1024] = {0,};
		cout << "> ";
		if (fgets(line, sizeof(line), stdin)) {
			line[strlen(line) - 1] = '\0';
			Var var = parse(line);
			cout << eval(var, env).toString() << endl;
		}
	}
}
