#include "Lisp.hpp"
#include "os.hpp"

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

	static Var proc(Var & func, vector<Var> args, Env & env);

	// builtin
	static void builtin_logic(Env & env);
	static void builtin_string(Env & env);
	static void builtin_artithmetic(Env & env);
	static void builtin_io(Env & env);
	static void builtin_file(Env & env);
	static void builtin_socket(Env & env);
	static void builtin_system(Env & env);
	static void builtin_date(Env & env);

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
					if (iter == s.end()) {
						throw "unexpected end of string";
					}
					if (*iter == '\\') {
						iter++;
					}
					str.append(1, *iter);
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
			} else if (symbol == "defun") {
				env[lv[1].getSymbol()] = Var(lv[2].getList(), lv[3].getList());
			} else if (symbol == "set") {
				env[lv[1].getSymbol()] = eval(lv[2], env);
			} else if (symbol == "if") {
				Var val = eval(lv[1], env);
				if (!val.nil() && val.getBoolean()) {
					return eval(lv[2], env);
				} else {
					return eval(lv[3], env);
				}
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
		builtin_logic(env);
		builtin_string(env);
		builtin_artithmetic(env);
		builtin_io(env);
		builtin_file(env);
		builtin_socket(env);
		builtin_system(env);
		builtin_date(env);
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

				string fmt = args[0].getString();
				size_t f = 0;
				size_t idx = 1;
				string match = "~a";
				while ((f = fmt.find(match, f)) != string::npos) {
					string rep = eval(args[idx++], env).toString();
					fmt.replace(f, match.length(), rep);
					f += rep.length();
				}

				return fmt;
			});
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE("=", ArithmeticEqual, {
				int val = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).getInteger()) {
						return false;
					}
				}
				return true;
			});
		DECL_NATIVE("+", Plus, {
				int sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum += eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("-", Minus, {
				int sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum -= eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("*", Multitude, {
				int sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum *= eval(*iter, env).getInteger();
				}
				return sum;
			});
		DECL_NATIVE("/", Divide, {
				int sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum /= eval(*iter, env).getInteger();
				}
				return sum;
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
				cout << eval(args[0], env).toString() << endl;
				return args[0];
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
				return (int)file.getSize();
			});
	}
	void builtin_socket(Env & env) {
	}
	void builtin_system(Env & env) {
		DECL_NATIVE("system", System, {
				return Var(system(eval(args[0], env).toString().c_str()));
			});
	}
	void builtin_date(Env & env) {
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
