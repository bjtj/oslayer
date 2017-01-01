#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "Iterator.hpp"
#include "FileStream.hpp"

#define HAS(M,E) (M.find(E) != M.end())
#define HEAP_ALLOC(E,V) E.alloc(new Var(V))
#define _VAR Obj<Var> 

#define DECL_NATIVE_BEGIN(NAME,CLS) \
	class CLS : public Procedure { \
	private: \
	public: \
	CLS(const string & name) : Procedure(name) {} \
	virtual ~CLS() {} \
	virtual _VAR proc(_VAR name, vector<_VAR> & args, Env & env) {
#define DECL_NATIVE_END(NAME,CLS) \
	} \
	}; \
	env[NAME] = env.alloc(new Var(AutoRef<Procedure>(new CLS(NAME))));

#define DECL_NATIVE(NAME,CLS,CODE) \
	DECL_NATIVE_BEGIN(NAME,CLS); \
	CODE; \
	DECL_NATIVE_END(NAME,CLS);

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;


	/**
	 * @brief 
	 */

	Env::Env() : parent(NULL), _quit(false) {}
	Env::Env(Env * parent) : parent(parent), _quit(false) {}
	Env::~Env() {
		_vars.clear();
		AutoLock lock(_heap.sem());
		_heap.clear();
	}
	bool Env::find (const string & name) {
		if ((_vars.find(name) == _vars.end()) == false) {
			return true;
		}
		if (parent && parent->find(name)) {
			return true;
		}
		return false;
	}
	OS::Obj<Var> & Env::get(const std::string & name) {
		if (parent && _vars.find(name) == _vars.end()) {
			return (*parent)[name];
		}
		if (_vars.find(name) == _vars.end()) {
			throw LispException("unbound - " + name);
		}
		return _vars[name];
		
	}
	_VAR & Env::operator[] (const string & name) {
		if (parent && _vars.find(name) == _vars.end()) {
			return (*parent)[name];
		}
		return _vars[name];
	}
	map<string, _VAR> & Env::root() {
		if (parent) {
			return parent->local();
		}
		return _vars;
	}
	map<string, _VAR> & Env::local() {
		return _vars;
	}
	void Env::quit(bool q) {
		_quit = q;
		if (parent) {
			parent->quit(q);
		}
	}
	bool Env::quit() {
		return _quit;
	}
	string Env::toString() {
		string ret;
		for (map<string, _VAR>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			ret.append(iter->first + " : " + iter->second->toString());
		}
		return ret;
	}
	SharedHeap<Var> & Env::heap() {
		if (parent) {
			return parent->heap();
		}
		return _heap;
	}
	_VAR Env::alloc(Var * var) {
		AutoLock lock(heap().sem());
		return heap().alloc(var);
	}
	void Env::gc() {
		AutoLock lock(_heap.sem());
		_heap.gc();
	}


	/**
	 * @brief 
	 */

	Func::Func() {
	}
	Func::Func(const _VAR & params, const _VAR & body) {
		_vars.push_back(params);
		_vars.push_back(body);
	}
	Func::~Func() {
	}
	_VAR & Func::params() {
		return _vars[0];
	}
	_VAR & Func::body() {
		return _vars[1];
	}
	_VAR Func::const_params() const {
		return _vars[0];
	}
	_VAR Func::const_body() const {
		return _vars[1];
	}
	bool Func::empty() {
		return _vars.size() < 2 || params()->isNil() || body()->isNil();
	}

	
	/**
	 * @brief 
	 */

	Var::Var() : type(NIL) {}
	Var::Var(const char * token) : type(NIL) {
		init(string(token));
	}
	Var::Var(const string & token) : type(NIL) {
		init(token);
	}
	Var::Var(vector<_VAR> lst) : type(LIST), lst(lst) {}
	Var::Var(bool bval) : type(BOOLEAN), bval(bval) {
		if (!bval) {
			type = NIL;
		}
	}
	Var::Var(const Boolean & bval) : type(BOOLEAN), bval(bval) {
		if (!bval.const_val()) {
			type = NIL;
		}
	}
	Var::Var(short inum) : type(INTEGER), inum(inum) {}
	Var::Var(int inum) : type(INTEGER), inum(inum) {}
	Var::Var(long inum) : type(INTEGER), inum(inum) {}
	Var::Var(long long inum) : type(INTEGER), inum(inum) {}
	Var::Var(const Integer & inum) : type(INTEGER), inum(inum) {}
	Var::Var(float fnum) : type(FLOAT), fnum(fnum) {}
	Var::Var(const Float & fnum) : type(FLOAT), fnum(fnum) {}
	Var::Var(const Func & func) : type(FUNC), func(func) {}
	Var::Var(AutoRef<Procedure> procedure) : type(FUNC), procedure(procedure) {}
	Var::Var(OS::File & file) : type(FILE), file(file) {}
	Var::Var(const FileDescriptor & fd) : type(FILE_DESCRIPTOR), fd(fd) {}
	Var::~Var() {}

	void Var::init(const string & token) {
		if (token.empty()) {
			throw LispException("empty token");
		}
		if (token == "nil") {
			type = NIL;
		} else if (token == "t") {
			type = BOOLEAN;
			bval = true;
		} else if (*token.begin() == '\"' && *token.rbegin() == '\"') {
			type = STRING;
			str = token;
		} else if (Integer::isIntegerString(token)) {
			type = INTEGER;
			inum = Integer::toInteger(token);
		} else if (Float::isFloatString(token)) {
			type = FLOAT;
			fnum = Float::toFloat(token);
		} else if (*token.begin() == '#' && *(token.begin() + 1) == 'p') {
			type = FILE;
			file = OS::File(token.substr(3, token.length() - 4));
		} else {
			type = SYMBOL;
			symbol = token;
		}
	}
	int Var::getType() { return type; }
	string Var::getTypeString() const {
		return getTypeString(type);
	}
	string Var::getTypeString(int type) const {
		switch (type) {
		case NIL:
			return "NIL";
		case SYMBOL:
			return "SYMBOL";
		case LIST:
			return "LIST";
		case BOOLEAN:
			return "BOOLEAN";
		case INTEGER:
			return "INTEGER";
		case FLOAT:
			return "FLOAT";
		case STRING:
			return "STRING";
		case FUNC:
			return "FUNCTION";
		case FILE:
			return "FILE";
		case FILE_DESCRIPTOR:
			return "FILE DESCRIPTOR";
		default:
			break;
		}
		throw LispException("unknown variable type / " + Text::toString(type));
	}
	void Var::checkTypeThrow(int t) const {
		if (type != t) {
			throw LispException(toString() + " / type not match (type: " + getTypeString() +
								", but required: " + getTypeString(t) + ")");
		}
	}
	bool Var::isNil() const {return type == NIL;}
	bool Var::isList() const {return type == LIST;}
	bool Var::isSymbol() const {return type == SYMBOL;}
	bool Var::isBoolean() const {return type == BOOLEAN;}
	bool Var::isInteger() const {return type == INTEGER;}
	bool Var::isFloat() const {return type == FLOAT;}
	bool Var::isString() const {return type == STRING;}
	bool Var::isFunction() const {return type == FUNC;}
	bool Var::isFile() const {return type == FILE;}
	bool Var::isFileDescriptor() const {return type == FILE_DESCRIPTOR;}
	string Var::getSymbol() const {checkTypeThrow(SYMBOL); return symbol;}
	string Var::getString() const {checkTypeThrow(STRING); return str;}
	vector<_VAR> & Var::getList() {checkTypeThrow(LIST); return lst;}
	Boolean Var::getBoolean() {checkTypeThrow(BOOLEAN); return bval;}
	Integer Var::getInteger() {checkTypeThrow(INTEGER); return inum;}
	Float Var::getFloat() {checkTypeThrow(FLOAT); return fnum;}
	OS::File & Var::getFile() {checkTypeThrow(FILE); return file;}
	Func Var::getFunc() {checkTypeThrow(FUNC); return func;}
	AutoRef<Procedure> Var::getProcedure() {checkTypeThrow(FUNC); return procedure;}
	FileDescriptor & Var::getFileDescriptor() {checkTypeThrow(FILE_DESCRIPTOR); return fd;}
	_VAR Var::proc(_VAR name, vector<_VAR> & args, Env & env) {

		if (!isFunction()) {
			throw LispException("not function / name: '" + name->toString() + "' / type : '" + getTypeString() + "'");
		}

		if (!procedure.nil()) {
			return procedure->proc(name, args, env);
		}

		Env e(&env);
		vector<_VAR> proto = getFunc().params()->getList();
		Arguments binder(proto);
		binder.mapArguments(e, e.local(), args);
		_VAR body = getFunc().body();
		return eval(body, e);
	}
	_VAR Var::proc(vector<_VAR> & args, Env & env) {
		if (!procedure.nil()) {
			return proc(HEAP_ALLOC(env, procedure->getName()), args, env);
		} else {
			return proc(HEAP_ALLOC(env, "nil"), args, env);
		}
	}

	string Var::toString() const {
		switch (type) {
		case NIL:
			return "NIL";
		case SYMBOL:
			return symbol;
		case LIST:
			{
				string ret = "(";
				for (vector<_VAR>::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
					if (iter != lst.begin()) {
						ret += " ";
					}
					ret += (*iter)->toString();
				}
				ret += ")";
				return ret;
			}
		case BOOLEAN:
			return bval.toString();
		case INTEGER:
			{
				char buffer[1024] = {0,};
				snprintf(buffer, sizeof(buffer), "%lld", inum.raw());
				return buffer;
			}
		case FLOAT:
			{
				char buffer[1024] = {0,};
				snprintf(buffer, sizeof(buffer), "%f", fnum.raw());
				return buffer;
			}
		case STRING:
			return untext(str);
		case FUNC:
			{
				if (!procedure.empty()) {
					return "#<COMPILED FUNCTION " + procedure->getName() + ">";
				}
				return "#<FUNCTION (PARAMS:" + func.const_params()->toString() +
					", BODY:" + func.const_body()->toString() + ")>";
			}
		case FILE:
			return "#p\"" + file.getPath() + "\"";
		case FILE_DESCRIPTOR:
			return "#<FD>";
		default:
			break;
		}
		throw LispException("unknown variable type / " + Text::toString(type));
	}

	// builtin
	static void builtin_type(Env & env);
	static void builtin_algorithm(Env & env);
	static void builtin_list(Env & env);
	static void builtin_logic(Env & env);
	static void builtin_string(Env & env);
	static void builtin_artithmetic(Env & env);
	static void builtin_io(Env & env);
	static void builtin_pathname(Env & env);
	static void builtin_file(Env & env);
	static void builtin_socket(Env & env);
	static void builtin_system(Env & env);
	static void builtin_date(Env & env);

	void validateArgumentCountMin(vector<_VAR> & args, size_t expect) {
		if (args.size() < expect) {
			throw LispException("Wrong argument count");
		}
	}

	static _VAR nil(Env & env) {
		return HEAP_ALLOC(env, "nil");
	}

	/**
	 *
	 */
	
	Arguments::Arguments() {}
	Arguments::Arguments(vector<_VAR> & proto) : proto(proto) {}
	Arguments::~Arguments() {}

	size_t Arguments::countPartArguments(vector<_VAR> & arr, size_t start) {
		size_t cnt = start;
		for (; cnt < arr.size(); cnt++) {
			if (Text::startsWith(arr[cnt]->getSymbol(), "&")) {
				break;
			}
		}
		return cnt;
	}
	void Arguments::mapArguments(Env & env, map<string, _VAR> & scope, vector<_VAR> & args) {

		size_t ec = countPartArguments(proto, 0);
		validateArgumentCountMin(args, ec);

		size_t ai = 0;
		size_t i = 0;
		for (; i < ec; i++, ai++) {
			_VAR val = eval(args[ai], env);
			scope[proto[i]->getSymbol()] = val;
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i]->getSymbol() == "&optional") {
			size_t offset = mapOptionals(env, scope, proto, ++i, args, ai);
			i += offset;
			ai += offset;
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i]->getSymbol() == "&rest") {
			if (i + 1 >= proto.size()) {
				throw LispException("Wrong function declaration");
			}
			_VAR val = HEAP_ALLOC(env, extractRest(env, args, ai));
			scope[proto[i + 1]->getSymbol()] = val;
		}

		_keywords = extractKeywords(args);
	}
	size_t Arguments::mapOptionals(Env & env, map<string, _VAR> & scope, vector<_VAR> & proto, size_t pstart, vector<_VAR> & args, size_t astart) {
		size_t i = pstart;
		size_t j = astart;
		for (; i < proto.size(); i++, j++) {

			if (proto[i]->isSymbol() && Text::startsWith(proto[i]->getSymbol(), "&")) {
				break;
			}

			string sym;
				
			if (proto[i]->isSymbol()) {
				sym = proto[i]->getSymbol();
				scope[sym] = nil(env);
			} else if (proto[i]->isList()) {
				validateArgumentCountMin(proto[i]->getList(), 2);
				sym = proto[i]->getList()[0]->getSymbol();
				scope[sym] = proto[i]->getList()[1];
			}

			if (j < args.size()) {
				_VAR val = eval(args[j], env);
				scope[sym] = val;
			}
		}
		return i - pstart;
	}
	vector<_VAR> Arguments::extractRest(Env & env, vector<_VAR> & args, size_t start) {
		vector<_VAR> rest;
		for (size_t i = start; i < args.size(); i++) {
			rest.push_back(eval(args[i], env));
		}
		return rest;
	}
	map<string, _VAR> Arguments::extractKeywords(vector<_VAR> & args) {
		map<string, _VAR> keywords;
		for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
			if ((*iter)->isSymbol() && Text::startsWith((*iter)->getSymbol(), ":")) {
				string name = (*iter)->getSymbol();
				_VAR val;
				if (iter + 1 != args.end()) {
					iter++;
					val = *iter;
				}
				keywords[name] = val;
			}
		}
		return keywords;
	}

	map<string, _VAR> & Arguments::keywords() {
		return _keywords;
	}

	static string format(Env & env, const string & fmt, vector<_VAR> & args) {
		string ret;
		size_t f = 0;
		size_t s = 0;
		Iterator<_VAR> iter(args);
		while ((f = fmt.find("~", f)) != string::npos) {
			if (f - s > 0) {
				ret.append(fmt.substr(s, f - s));
			}
			if (fmt[f + 1] == '%') {
				ret.append("\n");
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'a') {
				ret.append(eval(iter.next(), env)->toString());
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'd') {
				string num = eval(iter.next(), env)->toString();
				ret.append(num);
				s = f = (f + 2);
			} else if (fmt[f + 1] == ':' && fmt[f + 2] == 'd') {
				string num = eval(iter.next(), env)->toString();
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

	bool isNumber(_VAR var) {
		return var->isInteger() || var->isFloat();
	}

	_VAR pathname(Env & env, _VAR path) {
		if (path->isFile()) {
			return path;
		}
		File file(path->toString());
		return HEAP_ALLOC(env, file);
	}

	string text(const string & txt) {
		return "\"" + txt + "\"";
	}
	
	string untext(const string & txt) {
		return txt.substr(1, txt.length() - 2);
	}

	vector<_VAR> listy(_VAR var) {
		if (var->isList()) {
			return var->getList();
		}
		vector<_VAR> ret;
		ret.push_back(var);
		return ret;
	}

	_VAR toFloat(Env & env, _VAR v) {
		if (v->isInteger()) {
			return HEAP_ALLOC(env, (float)(*v->getInteger()));
		}
		return v;
	}

	bool eq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->getFloat() == v2->getFloat();
		}
		return v1->getInteger() == v2->getInteger();
	}

	bool gt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->getFloat() > v2->getFloat();
		}
		return v1->getInteger() > v2->getInteger();
	}

	bool lt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->getFloat() < v2->getFloat();
		}
		return v1->getInteger() < v2->getInteger();
	}

	bool gteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->getFloat() >= v2->getFloat();
		}
		return v1->getInteger() >= v2->getInteger();
	}

	bool lteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->getFloat() <= v2->getFloat();
		}
		return v1->getInteger() <= v2->getInteger();
	}

	_VAR plus(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return HEAP_ALLOC(env, v1->getFloat() + v2->getFloat());
		}
		return HEAP_ALLOC(env, v1->getInteger() + v2->getInteger());
	}

	_VAR minus(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return HEAP_ALLOC(env, v1->getFloat() - v2->getFloat());
		}
		return HEAP_ALLOC(env, v1->getInteger() - v2->getInteger());
	}

	_VAR multiply(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return HEAP_ALLOC(env, v1->getFloat() * v2->getFloat());
		}
		return HEAP_ALLOC(env, v1->getInteger() * v2->getInteger());
	}

	_VAR divide(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return HEAP_ALLOC(env, v1->getFloat() / v2->getFloat());
		}
		return HEAP_ALLOC(env, v1->getInteger() / v2->getInteger());
	}

	_VAR function(_VAR & var, Env & env) {
		if (var->isFunction()) {
			return var;
		}
		if (var->isSymbol()) {
			if (!env.get(var->getSymbol())->isFunction()) {
				throw LispException("invalid function - '" + var->getSymbol() + "'");
			}
			return env[var->getSymbol()];
		}
		_VAR func = eval(var, env);
		if (func->isFunction()) {
			return func;
		}
		throw LispException("invalid function - '" + var->toString() + "'");
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

	vector<string> tokenize(const string & s) {
		vector<string> tokens;
		for (string::const_iterator iter = s.begin(); iter != s.end(); iter++) {
			if (*iter == '\"') {
				string str;
				if (++iter == s.end()) {
					throw LispException("unexpected end of string");
				}
				for (;;iter++) {
					if (iter == s.end()) {
						throw LispException("unexpected end of string");
					}
					char ch = *iter;
					if (ch == '\"') {
						break;
					}
					if (ch == '\\') {
						if (++iter == s.end()) {
							throw LispException("unexpected end of string");
						}
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

	_VAR read_from_tokens(Env & env, vector<string>::iterator & iter, vector<string>::iterator & end) {

		if (iter == end) {
			throw LispException("syntax error - unexpected EOF");
		}
		if (*iter == "(") {
			vector<_VAR> lst;
			iter++;
			for (;*iter != ")"; iter++) {
				_VAR var = read_from_tokens(env, iter, end);
				lst.push_back(var);
			}
			return HEAP_ALLOC(env, lst);
		} else if (*iter == ")") {
			throw LispException("syntax error - unexpected )");
		} else {
			return HEAP_ALLOC(env, *iter);
		}
	}

	_VAR parse(Env & env, const string & cmd) {
		vector<string> tok = tokenize(cmd);
		vector<string>::iterator iter = tok.begin();
		vector<string>::iterator end = tok.end();
		return read_from_tokens(env, iter, end);
	}

	bool silentsymboleq(_VAR & var, const string & sym) {
		if (var->isSymbol()) {
			return var->getSymbol() == sym;
		}
		return false;
	}

	_VAR eval(_VAR var, Env & env) {
	
		if (var->isSymbol()) {
			if (!env.find(var->getSymbol()) || env.get(var->getSymbol())->isFunction()) {
				throw LispException("unbound variable - '" + var->getSymbol() + "'");
			}
			return env[var->getSymbol()];
		} else if (!var->isList()) {
			return var;
		} else if (var->getList().empty()) {
			return nil(env);
		} else {
			vector<_VAR> & lv = var->getList();
			if (silentsymboleq(lv[0], "quit")) {
				env.quit(true);
                return nil(env);
			} else if (silentsymboleq(lv[0], "lambda")) {
				validateArgumentCountMin(lv, 3);
				lv[1]->checkTypeThrow(Var::LIST);
				return HEAP_ALLOC(env, Func(lv[1], lv[2]));
			} else if (silentsymboleq(lv[0], "defun")) {
				validateArgumentCountMin(lv, 4);
				lv[2]->checkTypeThrow(Var::LIST);
				env[lv[1]->getSymbol()] = HEAP_ALLOC(env, Func(lv[2], lv[3]));
				return HEAP_ALLOC(env, lv[1]->getSymbol());
			} else if (silentsymboleq(lv[0], "setf")) {
				validateArgumentCountMin(lv, 3);
				_VAR var = eval(lv[1], env);
				_VAR other = eval(lv[2], env);
				if (var->isList() && other->isList()) {
					for (size_t i = 0; i < var->getList().size() && i < other->getList().size(); i++) {
						(*var->getList()[i]) = (*other->getList()[i]);
					}
				} else {
					*var = *other;
				}
				return var;
			} else if (silentsymboleq(lv[0], "setq")) {
				validateArgumentCountMin(lv, 3);
				_VAR val = eval(lv[2], env);
				env[lv[1]->getSymbol()] = val;
				return val;
			} else if (silentsymboleq(lv[0], "quote")) {
				validateArgumentCountMin(lv, 2);
				return lv[1];
				// TODO: implement backquote (quasiquote)
			} else if (silentsymboleq(lv[0], "function")) {
				validateArgumentCountMin(lv, 2);
				return function(lv[1], env);
			} else if (silentsymboleq(lv[0], "funcall")) {
				validateArgumentCountMin(lv, 2);
				_VAR funcsym = eval(lv[1], env);
				_VAR func = function(funcsym, env);
				vector<_VAR> args(lv.begin() + 2, lv.end());
				return func->proc(args, env);
			} else if (silentsymboleq(lv[0], "let")) {
				validateArgumentCountMin(lv, 2);
				_VAR ret = nil(env);
				vector<_VAR> & lets = lv[1]->getList();
				Env e(&env);
				for (vector<_VAR>::iterator iter = lets.begin(); iter != lets.end(); iter++) {
					vector<_VAR> decl = (*iter)->getList();
					string sym = decl[0]->getSymbol();
					e.local()[sym] = eval(decl[1], env);
				}
				for (vector<_VAR>::iterator iter = lv.begin() + 2; iter != lv.end(); iter++) {
					ret = eval(*iter, e);
				}
				return ret;
			} else if (silentsymboleq(lv[0], "if")) {
				validateArgumentCountMin(lv, 3);
				_VAR val = eval(lv[1], env);
				if (!val->isNil()) {
					return eval(lv[2], env);
				} else if (lv.size() > 3) {
					return eval(lv[3], env);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "when")) {
				validateArgumentCountMin(lv, 3);
				_VAR test = eval(lv[1], env);
				if (!test->isNil()) {
					return eval(lv[2], env);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "unless")) {
				validateArgumentCountMin(lv, 3);
				_VAR test = eval(lv[1], env);
				if (test->isNil()) {
					return eval(lv[2], env);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "cond")) {
				validateArgumentCountMin(lv, 1);
				for (vector<_VAR>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					vector<_VAR> lst = (*iter)->getList();
					if (!eval(lst[0], env)->isNil()) {
						return eval(lst[1], env);
					}
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "progn")) {
				validateArgumentCountMin(lv, 1);
				_VAR ret = nil(env);
				for (vector<_VAR>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					ret = eval(*iter, env);
				}
				return ret;
			} else if (silentsymboleq(lv[0], "while")) {
				validateArgumentCountMin(lv, 3);
				_VAR pre_test = lv[1];
				while (!eval(pre_test, env)->isNil()) {
					eval(lv[2], env);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "dolist")) {
				validateArgumentCountMin(lv, 3);
				Env e(&env);
				vector<_VAR> decl = lv[1]->getList();
				string param = decl[0]->getSymbol();
				vector<_VAR> lst = eval(decl[1], env)->getList();
				for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
					e.local()[param] = *iter;
					eval(lv[2], e);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "dotimes")) {
				validateArgumentCountMin(lv, 3);
				Env e(&env);
				vector<_VAR> steps = lv[1]->getList();
				string sym = steps[0]->getSymbol();
				Integer limit = eval(steps[1], env)->getInteger();
				e.local()[sym] = HEAP_ALLOC(env, Integer(0));
				for (; e.get(sym)->getInteger() < limit; e[sym] = HEAP_ALLOC(env, e.get(sym)->getInteger() + 1)) {
					eval(lv[2], e);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "loop")) {
				
				throw LispException("not implemeneted");
				
				// TODO: implement
				// [http://www.ai.sri.com/pkarp/loop.html]
				// code:
				//  (loop for x in '(a b c d e) do (print x))
				//
				// result:
				//  A
				//  B
				//  C
				//  D
				//  E
				//  NIL
				
			} else if (silentsymboleq(lv[0], "list")) {
				validateArgumentCountMin(lv, 1);
				vector<_VAR> elts;
				for (vector<_VAR>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					elts.push_back(eval(*iter, env));
				}
				return HEAP_ALLOC(env, elts);
			} else if (silentsymboleq(lv[0], "cons")) {
				validateArgumentCountMin(lv, 3);
				vector<_VAR> ret;
				_VAR cons = eval(lv[1], env);
				_VAR cell = eval(lv[2], env);
				ret.push_back(cons);
				if (cell->isList()) {
					vector<_VAR> lst = cell->getList();
					ret.insert(ret.end(), lst.begin(), lst.end());
				} else {
					ret.push_back(cell);
				}
				return HEAP_ALLOC(env, ret);
			} else if (silentsymboleq(lv[0], "car")) {
				validateArgumentCountMin(lv, 2);
				vector<_VAR> & lst = eval(lv[1], env)->getList();
				if (lst.size() > 0) {
					return lst[0];
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "cdr")) {
				validateArgumentCountMin(lv, 2);
				vector<_VAR> & lst = eval(lv[1], env)->getList();
				if (lst.size() > 1) {
					vector<_VAR> rest;
					for (vector<_VAR>::iterator iter = lst.begin() + 1; iter != lst.end(); iter++) {
						rest.push_back(*iter);
					}
					return HEAP_ALLOC(env, rest);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "nth")) {
				validateArgumentCountMin(lv, 3);
				size_t idx = (size_t)(*(eval(lv[1], env)->getInteger()));
				vector<_VAR> & lst = eval(lv[2], env)->getList();
				if (idx < lst.size()) {
					return lst[idx];
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "nthcdr")) {
				validateArgumentCountMin(lv, 3);
				size_t idx = (size_t)(*eval(lv[1], env)->getInteger());
				vector<_VAR> & lst = eval(lv[2], env)->getList();
				if (idx < lst.size()) {
					vector<_VAR> rest;
					for (vector<_VAR>::iterator iter = lst.begin() + idx; iter != lst.end(); iter++) {
						rest.push_back(*iter);
					}
					return HEAP_ALLOC(env, rest);
				}
				return nil(env);
			} else if (silentsymboleq(lv[0], "subseq")) {
				validateArgumentCountMin(lv, 4);
				vector<_VAR> & lst = eval(lv[1], env)->getList();
				Integer start = eval(lv[2], env)->getInteger();
				Integer end = eval(lv[3], env)->getInteger();
				vector<_VAR> ret;
				for (size_t i = (size_t)*start; i < (size_t)*end && i < lst.size(); i++) {
					ret.push_back(lst[i]);
				}
				return HEAP_ALLOC(env, ret);
			} else if (silentsymboleq(lv[0], "defmacro")) {
			
				throw LispException("not implemeneted");
				
				// TODO: implement
				// refer [http://clhs.lisp.se/Body/m_defmac.htm]
				
			} else if (silentsymboleq(lv[0], "macroexpand")) {
				
				throw LispException("not implemeneted");
			
				// TODO: implement
				
			} else {
				vector<_VAR> args(lv.begin() + 1, lv.end());
				_VAR func = function(lv[0], env);
				return func->proc(lv[0], args, env);
			}
		}

		return nil(env);
	}

	_VAR compile(const string & cmd, Env & env) {
		_VAR ret = eval(parse(env, BufferedCommandReader::trimComment(cmd)), env);
		env.gc();
		return ret;
	}

	void native(Env & env) {
		builtin_type(env);
		builtin_algorithm(env);
		builtin_list(env);
		builtin_logic(env);
		builtin_string(env);
		builtin_artithmetic(env);
		builtin_io(env);
		builtin_pathname(env);
		builtin_file(env);
		builtin_socket(env);
		builtin_system(env);
		builtin_date(env);
	}

	void builtin_type(Env & env) {
		DECL_NATIVE("symbolp", Symbolp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isSymbol());
			});
		DECL_NATIVE("listp", Listp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isList());
			});
		DECL_NATIVE("booleanp", Booleanp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isBoolean());
			});
		DECL_NATIVE("integerp", Integerp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isInteger());
			});
		DECL_NATIVE("floatp", Floatp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isFloat());
			});
		DECL_NATIVE("stringp", Stringp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isString());
			});
		DECL_NATIVE("funcp", Funcp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isFunction());
			});
		DECL_NATIVE("pathnamep", Pathnamep, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isFile());
			});
		DECL_NATIVE("streamp", Streamp, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isFileDescriptor());
			});
	}

	void builtin_algorithm(Env & env) {

		// remove

		// TODO: refer - [http://www.lispworks.com/documentation/lw60/CLHS/Body/f_map.htm]
		DECL_NATIVE("map", Map, {
				validateArgumentCountMin(args, 3);
				_VAR sym = eval(args[0], env); /* TODO: use it */
				_VAR func = eval(args[1], env);
				func = function(func, env);
				_VAR seq = eval(args[2], env); /* TODO: use it */

				vector<_VAR> ret;

				vector<vector<_VAR> > lists;
				size_t size = 0;
				for (size_t i = 2; i < args.size(); i++) {
					vector<_VAR> lst = eval(args[i], env)->getList();
					if (lst.size() > size) {
						size = lst.size();
					}
					lists.push_back(lst);
				}

				for (size_t i = 0; i < size; i++) {
					vector<_VAR> fargs;
					for (vector<vector<_VAR> >::iterator iter = lists.begin(); iter != lists.end(); iter++) {
						vector<_VAR> & lst = (*iter);
						fargs.push_back((i < lst.size() ? lst[i] : nil(env)));
					}
					ret.push_back(func->proc(fargs, env));
				}

				return HEAP_ALLOC(env, ret);
			});

		DECL_NATIVE("sort", Sort, {
				validateArgumentCountMin(args, 2);
				vector<_VAR> lst = eval(args[0], env)->getList();
				_VAR func = eval(args[1], env);
				func = function(func, env);

				if (lst.size() <= 1) {
					return HEAP_ALLOC(env, lst);
				}

				for (size_t loop = 0; loop < lst.size() - 1; loop++) {
					for (size_t i = 0; i < lst.size() - 1; i++) {
						vector<_VAR> fargs;
						fargs.push_back(lst[i]);
						fargs.push_back(lst[i + 1]);
						if (!func->proc(HEAP_ALLOC(env, "#sort"), fargs, env)->isNil()) {
							iter_swap(lst.begin() + i, lst.begin() + (i + 1));
						}
					}
				}
				return HEAP_ALLOC(env, lst);
			});
	}

	void builtin_list(Env & env) {
		DECL_NATIVE("length", Length, {
				validateArgumentCountMin(args, 1);
				vector<_VAR> lst = eval(args[0], env)->getList();
				return HEAP_ALLOC(env, Integer((long long)lst.size()));
			});
		
		DECL_NATIVE("append", Append, {
				validateArgumentCountMin(args, 0);
				vector<_VAR> ret;
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					vector<_VAR> lst = eval(*iter, env)->getList();
					ret.insert(ret.end(), lst.begin(), lst.end());
				}
				return HEAP_ALLOC(env, ret);
			});
		
		DECL_NATIVE("remove", Remove, {
				validateArgumentCountMin(args, 2);
				_VAR val = eval(args[0], env);
				vector<_VAR> lst = eval(args[1], env)->getList();
				for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
					if (val->toString() == (*iter)->toString()) {
						iter = lst.erase(iter);
					} else {
						iter++;
					}
				}
				return HEAP_ALLOC(env, lst);
			});
        
        class RemoveIf : public Procedure {
        private:
        public:
            RemoveIf(const string & name) : Procedure(name) {}
            virtual ~RemoveIf() {}
            virtual _VAR proc(_VAR name, vector<_VAR> & args, Env & env) {
                validateArgumentCountMin(args, 2);
                _VAR func = eval(args[0], env);
				func = function(func, env);
                vector<_VAR> lst = eval(args[1], env)->getList();
                for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
                    vector<_VAR> fargs;
                    fargs.push_back(*iter);
                    if (!func->proc(fargs, env)->isNil()) {
                        iter = lst.erase(iter);
                    } else {
                        iter++;
                    }
                }
                return HEAP_ALLOC(env, lst);
            }
        };
        env["remove-if"] = HEAP_ALLOC(env, AutoRef<Procedure>(new RemoveIf("remove-if")));

		// TODO: implement
		// [https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html]
		// remove-if-not
		// [http://www.jtra.cz/stuff/lisp/sclr/push.html]
		// push
		// pop
	}

	void builtin_logic(Env & env) {
		DECL_NATIVE("not", Not, {
				validateArgumentCountMin(args, 1);
				return HEAP_ALLOC(env, eval(args[0], env)->isNil());
			});

		DECL_NATIVE("or", Or, {
				validateArgumentCountMin(args, 0);
				_VAR var = nil(env);
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (!var->isNil()) {
						break;
					}
				}
				return var;
			});

		DECL_NATIVE("and", And, {
				validateArgumentCountMin(args, 0);
				_VAR var = HEAP_ALLOC(env, "t");
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (var->isNil()) {
						break;
					}
				}
				return var;
			});
	}

	void builtin_string(Env & env) {

		DECL_NATIVE("string=", LiteralEqual, {
				validateArgumentCountMin(args, 1);
				string val = eval(args[0], env)->toString();
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env)->toString()) {
						return nil(env);
					}
				}
				return HEAP_ALLOC(env, true);
			});

		DECL_NATIVE("string-prefix-p", StringPrefixP, {
				validateArgumentCountMin(args, 2);
				string str = eval(args[0], env)->toString();
				string dst = eval(args[1], env)->toString();
				return HEAP_ALLOC(env, Text::startsWith(str, dst));
			});

		DECL_NATIVE("string-suffix-p", StringSuffixP, {
				validateArgumentCountMin(args, 2);
				string str = eval(args[0], env)->toString();
				string dst = eval(args[1], env)->toString();
				return HEAP_ALLOC(env, Text::endsWith(str, dst));
			});

		DECL_NATIVE("string-length", StringLength, {
				validateArgumentCountMin(args, 1);
				Integer len((long long)eval(args[0], env)->toString().length());
				return HEAP_ALLOC(env, len);
			});

		DECL_NATIVE("string-append", StringAppend, {
				validateArgumentCountMin(args, 0);
				string ret;
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					ret.append(eval(*iter, env)->toString());
				}
				return HEAP_ALLOC(env, text(ret));
			});

		DECL_NATIVE("format", Format, {
				validateArgumentCountMin(args, 2);
				_VAR test = eval(args[0], env);
				vector<_VAR> fargs(args.begin() + 2, args.end());
				string str = format(env, args[1]->toString(), fargs);
				if (!test->isNil()) {
					fputs(str.c_str(), stdout);
					fputs("\n", stdout);
					return nil(env);
				}
				return HEAP_ALLOC(env, text(str));
			});
		
		DECL_NATIVE("enough-namestring", EnoughNamestring, {
				validateArgumentCountMin(args, 2);
				string org = eval(args[0], env)->toString();
				string prefix = eval(args[1], env)->toString();
				if (Text::startsWith(org, prefix)) {
					return HEAP_ALLOC(env, text(org.substr(prefix.length())));
				}
				return HEAP_ALLOC(env, text(org));
			});
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE("=", ArithmeticEqual, {
				validateArgumentCountMin(args, 1);
				_VAR v = eval(args[0], env);
				Integer val = v->getInteger();
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (!eq(env, v, eval(*iter, env))) {
						return nil(env);
					}
				}
				return HEAP_ALLOC(env, true);
			});
		DECL_NATIVE("+", Plus, {
				_VAR v = HEAP_ALLOC(env, 0);
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					v = plus(env, v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("-", Minus, {
				validateArgumentCountMin(args, 1);				
				if (args.size() == 1) {
					return minus(env, HEAP_ALLOC(env, 0), eval(args[0], env));
				}
				_VAR v = eval(args[0], env);
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					v = minus(env, v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("*", Multitude, {
				_VAR v = HEAP_ALLOC(env, 1);
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					v = multiply(env, v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("/", Divide, {
				validateArgumentCountMin(args, 1);
				_VAR v = eval(args[0], env);
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					v = divide(env, v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("%", Rest, {
				validateArgumentCountMin(args, 1);
				Integer sum = eval(args[0], env)->getInteger();
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum %= eval(*iter, env)->getInteger();
				}
				return HEAP_ALLOC(env, sum);
			});

		DECL_NATIVE(">", Greater, {
				validateArgumentCountMin(args, 2);
				return HEAP_ALLOC(env, gt(env, eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE("<", Less, {
				validateArgumentCountMin(args, 2);
				return HEAP_ALLOC(env, lt(env, eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE(">=", GreaterEq, {
				validateArgumentCountMin(args, 2);
				return HEAP_ALLOC(env, gteq(env, eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE("<=", LessEq, {
				validateArgumentCountMin(args, 2);
				return HEAP_ALLOC(env, lteq(env, eval(args[0], env), eval(args[1], env)));
			});
	}
	void builtin_io(Env & env) {

		env["*standard-output*"] = HEAP_ALLOC(env, FileDescriptor(stdout));
		env["*standard-input*"] = HEAP_ALLOC(env, FileDescriptor(stdin));
		
        class Read : public Procedure {
        public:
            Read(const string & name) : Procedure(name) {}
            virtual ~Read() {}
            virtual _VAR proc(_VAR name, vector<_VAR> & args, Env & env) {
                validateArgumentCountMin(args, 1);
                _VAR ret = nil(env);
                FileDescriptor fd = eval(args[0], env)->getFileDescriptor();
                if (fd.eof()) {
                    return HEAP_ALLOC(env, true);
                }
                BufferedCommandReader reader;
                while (!fd.eof() && reader.read(fd.readline() + "\n") < 1) {}
                
                vector<string> commands = reader.getCommands();
                for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
                    ret = compile(*iter, env);
                }
                return ret;
            }
        };
        env["read"] = HEAP_ALLOC(env, AutoRef<Procedure>(new Read("read")));

		DECL_NATIVE("read-line", ReadLine, {
				validateArgumentCountMin(args, 1);
				FileDescriptor fd = eval(args[0], env)->getFileDescriptor();
				if (fd.eof()) {
					return HEAP_ALLOC(env, true);
				}
				string line = fd.readline();
				return HEAP_ALLOC(env, text(line));
			});
		DECL_NATIVE("print", Print, {
				validateArgumentCountMin(args, 1);
				FileDescriptor fd = env.get("*standard-output*")->getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env)->getFileDescriptor();
				}
				string msg = eval(args[0], env)->toString();
				fd.write(msg);
				fd.write("\n");
				return HEAP_ALLOC(env, text(msg));
			});

		DECL_NATIVE("write-string", WriteString, {
				validateArgumentCountMin(args, 1);
				FileDescriptor fd = env.get("*standard-output*")->getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env)->getFileDescriptor();
				}
				string msg = eval(args[0], env)->toString();
				fd.write(msg);
				return HEAP_ALLOC(env, text(msg));
			});

		DECL_NATIVE("write-line", WriteLine, {
				validateArgumentCountMin(args, 1);
				FileDescriptor fd = env.get("*standard-output*")->getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env)->getFileDescriptor();
				}
				string msg = eval(args[0], env)->toString();
				fd.write(msg);
				fd.write("\n");
				return HEAP_ALLOC(env, text(msg));
			});
	}
	void builtin_pathname(Env & env) {
		DECL_NATIVE("pathname", Pathname, {
				validateArgumentCountMin(args, 1);
				_VAR path = pathname(env, eval(args[0], env));
				return path;
			});
		DECL_NATIVE("pathname-name", PathnameName, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, text(file.getFileNameWithoutExtension()));
			});
		DECL_NATIVE("pathname-type", PathnameType, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, text(file.getExtension()));
			});
		DECL_NATIVE("namestring", Namestring, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, text(file.getPath()));
			});
		DECL_NATIVE("directory-namestring", DirectoryNamestring, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, text(file.getDirectory()));
			});
		DECL_NATIVE("file-namestring", FileNamestring, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, text(file.getFileName()));
			});
		
		// https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html
		
		DECL_NATIVE("directory-file-name", DirectoryFileName, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				string path = file.getPath();
				if (path.empty()) {
					return nil(env);
				}
				if (File::getSeparators().find(*path.rbegin()) != string::npos) {
					return HEAP_ALLOC(env, text(path.substr(0, path.size() - 1)));
				}
				return HEAP_ALLOC(env, text(path));
			});
		DECL_NATIVE("file-name-directory", FileNameDirectory, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				string path = file.getPath();
				size_t f = path.find_last_of(File::getSeparators());
				if (f == string::npos) {
					return nil(env);
				}
				return HEAP_ALLOC(env, text(path.substr(0, f+1)));
			});
	}
	void builtin_file(Env & env) {
		DECL_NATIVE("dir", Dir, {
				validateArgumentCountMin(args, 0);
				_VAR path = ((args.size() > 0) ? pathname(env, eval(args[0], env)) : HEAP_ALLOC(env, "#p\".\""));
				vector<File> files = File::list(path->getFile().getPath());
				vector<_VAR> lst;
				for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
					lst.push_back(HEAP_ALLOC(env, *iter));
				}
				return HEAP_ALLOC(env, lst);
			});
		DECL_NATIVE("probe-file", ProbeFile, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return file.exists() ? HEAP_ALLOC(env, file) : nil(env);
			});
		DECL_NATIVE("dirp", Dirp, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, file.isDirectory());
			});
		DECL_NATIVE("filep", Filep, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, file.isFile());
			});
		DECL_NATIVE("file-length", FileLength, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, Integer((long long)file.getSize()));
			});
		DECL_NATIVE("file-attribute-creation", FileAttributeCreation, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.creationTime()).sec));
			});
		DECL_NATIVE("file-attribute-lastmodified", FileAttributeLastModified, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				return HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.lastModifiedTime()).sec));
			});

		class Open : public Procedure {
		public:
			Open(const string & name) : Procedure(name) {}
			virtual ~Open() {}
			virtual _VAR proc(_VAR name, vector<_VAR> & args, Env & env) {
				map<string, _VAR> keywords = Arguments::extractKeywords(args);
				File file = pathname(env, eval(args[0], env))->getFile();
				const char * flags = "rb+";
				if (!file.exists()) {
					// does not exists
					if (HAS(keywords, ":if-does-not-exist")) {
						if (keywords[":if-does-not-exist"]->isNil()) {
							return nil(env);
						} else if (keywords[":if-does-not-exist"]->getSymbol() == ":create") {
							flags = "wb+";
						}
					}
				} else {
					// exists
					if (HAS(keywords, ":if-exists")) {
						if (keywords[":if-exists"]->isNil()) {
							return nil(env);
						} else if (keywords[":if-exists"]->getSymbol() == ":append") {
							flags = "ab+";
						} else if (keywords[":if-exists"]->getSymbol() == ":overwrite") {
							flags = "wb+";
						}
					}
				}
				
				// TODO: refactoring
#if defined(USE_UNIX_STD)
				FILE * fp = fopen(file.getPath().c_str(), flags);
				if (!fp) {
					throw LispException("Cannot open file");
				}
				return HEAP_ALLOC(env, FileDescriptor(fp));
#elif defined(USE_MS_WIN)
				FILE * fp = NULL;
				if (fopen_s(&fp, file.getPath().c_str(), flags) != 0) {
					throw LispException("Cannot open file");
				}
				return HEAP_ALLOC(env, FileDescriptor(fp));
#endif		
			}
		};
		env["open"] = HEAP_ALLOC(env, AutoRef<Procedure>(new Open("open")));

		DECL_NATIVE("file-position", FilePosition, {
				validateArgumentCountMin(args, 1);
				FileDescriptor fd = eval(args[0], env)->getFileDescriptor();
				if (args.size() > 1) {
					fd.position((size_t)*eval(args[1], env)->getInteger());
				}
				return HEAP_ALLOC(env, Integer((long long)fd.position()));
				
			});
		
		DECL_NATIVE("close", Close, {
				validateArgumentCountMin(args, 1);
				eval(args[0], env)->getFileDescriptor().close();
				return nil(env);
			});
	}
	void builtin_socket(Env & env) {
		// TODO: implement
	}
	void builtin_system(Env & env) {
		DECL_NATIVE("system", System, {
				validateArgumentCountMin(args, 1);
				Integer ret(system(eval(args[0], env)->toString().c_str()));
				return HEAP_ALLOC(env, ret);
			});
		DECL_NATIVE("load", Load, {
				validateArgumentCountMin(args, 1);
				File file = pathname(env, eval(args[0], env))->getFile();
				FileStream stream(file, "rb");
				string dump = stream.readFullAsString();
				stream.close();
				vector<string> lines = Text::split(dump, "\n");
				BufferedCommandReader reader;
				for (vector<string>::iterator iter = lines.begin(); !env.quit() && iter != lines.end(); iter++) {
					if (reader.read(*iter + "\n") > 0) {
						vector<string> commands = reader.getCommands();
						for (vector<string>::iterator cmd = commands.begin(); !env.quit() && cmd != commands.end(); cmd++) {
							compile(*cmd, env);
						}
					}
				}
				return HEAP_ALLOC(env, true);
			});
	}
	
	void builtin_date(Env & env) {
		env["internal-time-units-per-second"] = HEAP_ALLOC(env, 1000);
		DECL_NATIVE("now", Now, {
				validateArgumentCountMin(args, 0);
				char buffer[512];
				memset(buffer, 0, sizeof(buffer));
				Date date = Date::now();
				snprintf(buffer, sizeof(buffer), "%04d-%02d-%02d %02d:%02d:%02d.%d",
						 date.getYear(), date.getMonth() + 1, date.getDay(),
						 date.getHour(), date.getMinute(), date.getSecond(),
						 date.getMillisecond());
				return HEAP_ALLOC(env, text(buffer));
			});
		DECL_NATIVE("get-universal-time", GetUniversalTime, {
				validateArgumentCountMin(args, 0);
				return HEAP_ALLOC(env, (long long)osl_get_time_network().sec);
			});
		DECL_NATIVE_BEGIN("decode-universal-time", DecodeUniversalTime);
		{
			validateArgumentCountMin(args, 1);
			// TODO: apply specific zone
			osl_time_t time = {0,};
			time.sec = (unsigned long)*eval(args[0], env)->getInteger();
			time = osl_network_time_to_system_time(time);
			Date date(time);
			vector<_VAR> ret;
			ret.push_back(HEAP_ALLOC(env, date.getSecond()));
			ret.push_back(HEAP_ALLOC(env, date.getMinute()));
			ret.push_back(HEAP_ALLOC(env, date.getHour()));
			ret.push_back(HEAP_ALLOC(env, date.getDay()));
			ret.push_back(HEAP_ALLOC(env, date.getMonth() + 1));
			ret.push_back(HEAP_ALLOC(env, date.getYear()));
			// day of week
			// daylight-p
			// zone
			return HEAP_ALLOC(env, ret);
		}
		DECL_NATIVE_END("decode-universal-time", DecodeUniversalTime);

		DECL_NATIVE_BEGIN("encode-universal-time", EncodeUniversalTime);
		{
			validateArgumentCountMin(args, 6); // seconds, minutes, hours, dates, month and year (gmt offset)
			Date date = Date::now();
			date.setSecond((int)*args[0]->getInteger());
			date.setMinute((int)*args[1]->getInteger());
			date.setHour((int)*args[2]->getInteger());
			date.setDay((int)*args[3]->getInteger());
			date.setMonth((int)((*args[4]->getInteger()) - 1));
			date.setYear((int)*args[5]->getInteger());
			if (args.size() > 6) {
				date.setGmtOffset((int)*args[6]->getInteger());
			}
			return HEAP_ALLOC(env, (long long)osl_system_time_to_network_time(date.getTime()).sec);
		}
		DECL_NATIVE_END("encode-universal-time", EncodeUniversalTime);
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
	string BufferedCommandReader::trimComment(const string & text) {
		string ret;
		bool in_text = false;
		bool in_comment = false;
		bool in_escape = false;
		for (size_t i = 0; i < text.size(); i++) {
			
			if (in_escape) {
				ret.append(1, text[i]);
				in_escape = false;
				continue;
			}

			if (in_comment) {
				if (text[i] == '\n') {
					ret.append(1, text[i]);
					in_comment = false;
				}
				continue;
			}
			
			switch (text[i]) {
			case '"':
				in_text = !in_text;
				break;
			case '\\':
				if (in_text) {
					in_escape = true;
				}
				break;
			case ';':
				if (!in_text) {
					in_comment = true;
				}
				break;
			default:
				break;
			}
			
			if (in_comment) {
				continue;
			}
			
			ret.append(1, text[i]);
		}
		return ret;
	}
	size_t BufferedCommandReader::testComplete(const string & text) {
		size_t brace_count = 0;
		bool in_text = false;
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
					throw LispException("unexpected end of command");
				}
				if (brace_count > 0) {
					brace_count--;
				}
				if (brace_count == 0) {
					return i + 1;
				}
				break;
			case '\\':
				if (in_text) {
					i++;
				}
				break;
			case '\"':
				in_text = !in_text;
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
		buffer = trimComment(buffer);
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

	string printVar(_VAR var) {
		if (var->isString()) {
			return var->getString();
		}
		return var->toString();
	}

	void repl(Env & env) {
		BufferedCommandReader reader;
		char line[1024] = {0,};
		fputs("> ", stdout);
		while (fgets(line, sizeof(line), stdin)) {
			if (reader.read(line) > 0) {
				vector<string> & commands = reader.getCommands();
				for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
					fputs(printVar(compile(*iter, env)).c_str(), stdout);
					fputs("\n", stdout);
				}
				reader.clearCommands();
				return;
			}
		}
	}
}
