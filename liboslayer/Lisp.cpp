#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "Iterator.hpp"
#include "FileReaderWriter.hpp"

#define DECL_NATIVE(NAME,CLS,CODE)										\
	class CLS : public Procedure {										\
	private:															\
	public:																\
	CLS(const string & name) : Procedure(name) {}						\
	virtual ~CLS() {}													\
	virtual Var proc(Var name, vector<Var> & args, Env & env) {CODE}	\
	};																	\
	env[NAME] = Var(AutoRef<Procedure>(new CLS(NAME)));

#define PUSH_AND_RETURN(ENV,VAR) do { ENV.push(VAR); return VAR; } while(0);

#define HAS(M,E) (M.find(E) != M.end())

#define HEAP_ALLOC(E,V) E.alloc(new Var(V))

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;


	/**
	 * @brief 
	 */

	Env::Env() : parent(NULL), _quit(false) {}
	Env::Env(Env * parent) : parent(parent), _quit(false) {}
	Env::~Env() {}
	bool Env::find (const string & name) {
		if ((_vars.find(name) == _vars.end()) == false) {
			return true;
		}
		if (parent && parent->find(name)) {
			return true;
		}
		return false;
	}
	Var & Env::operator[] (const string & name) {
		if (parent && _vars.find(name) == _vars.end()) {
			return (*parent)[name];
		}
		return _vars[name];
	}
	map<string, Var> & Env::root() {
		if (parent) {
			return parent->local();
		}
		return _vars;
	}
	map<string, Var> & Env::local() {
		return _vars;
	}
	vector<Var> & Env::stack() {
		if (parent) {
			return parent->stack();
		}
		return _stack;
	}
	void Env::push(Var var) {
		stack().push_back(var);
	}
	Var Env::pop() {
		Var ret = *(stack().rbegin());
		stack().erase(stack().begin() + stack().size() - 1);
		return ret;
	}
	Var & Env::last() {
		if (stack().size() == 0) {
			throw LispException("empty stack");
		}
		return *(stack().rbegin());
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
		for (map<string, Var>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			ret.append(iter->first + " : " + iter->second.toString());
		}
		return ret;
	}
	Heap<Var> & Env::heap() {
		return _heap;
	}
	Obj<Var> Env::alloc(Var * var) {
		return _heap.alloc(var);
	}
	void Env::gc() {
		_heap.gc();
	}


	/**
	 * @brief 
	 */

	Func::Func() {
	}
	Func::Func(const Var & params, const Var & body) {
		_vars.push_back(params);
		_vars.push_back(body);
	}
	Func::~Func() {
	}
	Var & Func::params() {
		return _vars[0];
	}
	Var & Func::body() {
		return _vars[1];
	}
	Var Func::const_params() const {
		return _vars[0];
	}
	Var Func::const_body() const {
		return _vars[1];
	}
	bool Func::empty() {
		return _vars.size() < 2 || params().isNil() || body().isNil();
	}

	/**
	 * @brief 
	 */

	RefVar::RefVar() : _ref(NULL) {
	}
	RefVar::RefVar(Var * ref) : _ref(dereference(ref)) {
		if (_ref) {
			testDoubleRefThrow();
		}
	}
	RefVar::~RefVar() {
	}
	Var * RefVar::dereference(Var * ref) {
		Var * r = ref;
		while (r && r->isRef()) {
			r = r->getRef().ref();
		}
		return r;
	}
	Var * RefVar::dereference() {
		return dereference(_ref);
	}
	bool RefVar::isNil() const {
		return _ref == NULL;
	}
	void RefVar::testNilThrow() const {
		if (isNil()) {
			throw LispException("exception: null reference");
		}
	}
	void RefVar::testDoubleRefThrow() const {
		if (_ref->isRef()) {
			throw LispException("exception: double reference");
		}
	}
	Var * RefVar::ref() {
		return _ref;
	}
	Var * RefVar::const_ref() const {
		return _ref;
	}
	Var & RefVar::operator* () {
		testNilThrow();
		return *_ref;
	}
	Var * RefVar::operator-> () {
		testNilThrow();
		return _ref;
	}
	RefVar & RefVar::operator= (const RefVar & other) {
		_ref = other._ref;
		return *this;
	}
	RefVar & RefVar::operator= (const Var & other) {
		*_ref = other;
		return *this;
	}
	string RefVar::toString() const {
		testNilThrow();
		testDoubleRefThrow();
		return _ref->toString();
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
	Var::Var(vector<Var> lst) : type(LIST), lst(lst) {}
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
	Var::Var(Var * refvar) : type(REF), refvar(refvar) {
		if (refvar == NULL) {
			type = NIL;
		}
	}
	Var::Var(const RefVar & refvar) : type(REF), refvar(refvar) {
		if (refvar.isNil()) {
			type = NIL;
		}
	}
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
		case REF:
			return "REFERENCE";
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
	bool Var::isRef() const {return type == REF;}
	bool Var::isFileDescriptor() const {return type == FILE_DESCRIPTOR;}
	string Var::getSymbol() const {checkTypeThrow(SYMBOL); return symbol;}
	string Var::getString() const {checkTypeThrow(STRING); return str;}
	vector<Var> & Var::getList() {checkTypeThrow(LIST); return lst;}
	Boolean Var::getBoolean() {checkTypeThrow(BOOLEAN); return bval;}
	Integer Var::getInteger() {checkTypeThrow(INTEGER); return inum;}
	Float Var::getFloat() {checkTypeThrow(FLOAT); return fnum;}
	OS::File & Var::getFile() {checkTypeThrow(FILE); return file;}
	Func Var::getFunc() {checkTypeThrow(FUNC); return func;}
	AutoRef<Procedure> Var::getProcedure() {checkTypeThrow(FUNC); return procedure;}
	RefVar Var::getRef() const {checkTypeThrow(REF); return refvar;}
	FileDescriptor & Var::getFileDescriptor() {checkTypeThrow(FILE_DESCRIPTOR); return fd;}
	Var Var::proc(Var name, vector<Var> & args, Env & env) {

		if (!isFunction()) {
			throw LispException("not function / name: '" + name.toString() + "' / type : '" + getTypeString() + "'");
		}

		if (!procedure.nil()) {
			return procedure->proc(name, args, env);
		}

		Env e(&env);
		vector<Var> proto = getFunc().params().getList();
		Arguments binder(proto);
		binder.mapArguments(e, e.local(), args);
		Var body = getFunc().body();
		return eval(body, e);
	}
	Var Var::proc(vector<Var> & args, Env & env) {
		if (!procedure.nil()) {
			return proc(Var(procedure->getName()), args, env);
		} else {
			return proc(Var("nil"), args, env);
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
				for (vector<Var>::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
					if (iter != lst.begin()) {
						ret += " ";
					}
					ret += iter->toString();
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
				return "#<FUNCTION (PARAMS:" + func.const_params().toString() +
					", BODY:" + func.const_body().toString() + ")>";
			}
		case FILE:
			return "#p\"" + file.getPath() + "\"";
		case REF:
			return "#REFERENCE/" + refvar.toString();
		case FILE_DESCRIPTOR:
			return "#<FD>";
		default:
			break;
		}
		throw LispException("unknown variable type / " + Text::toString(type));
	}

	Var & Var::operator* () {
		if (type == REF) {
			return *refvar;
		}
		return *this;
	}

	Var & Var::operator= (const Var & other) {
			
		this->symbol = other.symbol;
		this->str = other.str;
		this->bval = other.bval;
		this->inum = other.inum;
		this->fnum = other.fnum;
		this->func = other.func;
		this->procedure = other.procedure;
		this->file = other.file;
		this->fd = other.fd;

		if (this->type == LIST && this->lst.size() > 0 && this->lst[0].isRef() && other.type == LIST) {
			vector<Var>::const_iterator o = other.lst.begin();
			for (vector<Var>::iterator i = this->lst.begin(); i != this->lst.end() && o != other.lst.end(); i++, o++) {
				*i = *o;
			}
		} else {
			this->lst.clear();
			if (other.type == LIST) {
				for (vector<Var>::const_iterator iter = other.lst.begin(); iter != other.lst.end(); iter++) {
					this->lst.push_back(iter->isRef() ? *(iter->getRef()) : *iter);
				}
			}
		}

		if (this->type == REF) {
			if (other.type == REF) {
				*(this->refvar) = *(other.refvar.const_ref());
			} else {
				*(this->refvar) = other;
			}
		} else if (other.type == REF) {
			(*this) = *(other.refvar.const_ref());
		} else {
			this->type = other.type;
		}
		return *this;
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

	void testArgumentCount(vector<Var> & args, size_t expect) {
		if (args.size() < expect) {
			throw LispException("Wrong argument count");
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
	void Arguments::mapArguments(Env & env, map<string, Var> & scope, vector<Var> & args) {

		size_t ec = countPartArguments(proto, 0);
		testArgumentCount(args, ec);

		size_t ai = 0;
		size_t i = 0;
		for (; i < ec; i++, ai++) {
			Var val = eval(args[ai], env);
			scope[proto[i].getSymbol()] = val;
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i].getSymbol() == "&optional") {
			size_t offset = mapOptionals(env, scope, proto, ++i, args, ai);
			i += offset;
			ai += offset;
		}

		if (i >= proto.size()) {
			return;
		}

		if (proto[i].getSymbol() == "&rest") {
			if (i + 1 >= proto.size()) {
				throw LispException("Wrong function declaration");
			}
			Var val = Var(extractRest(env, args, ai));
			scope[proto[i + 1].getSymbol()] = val;
		}

		_keywords = extractKeywords(args);
	}
	size_t Arguments::mapOptionals(Env & env, map<string, Var> & scope, vector<Var> & proto, size_t pstart, vector<Var> & args, size_t astart) {
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
				Var val = eval(args[j], env);
				scope[sym] = val;
			}
		}
		return i - pstart;
	}
	vector<Var> Arguments::extractRest(Env & env, vector<Var> & args, size_t start) {
		vector<Var> rest;
		for (size_t i = start; i < args.size(); i++) {
			rest.push_back(eval(args[i], env));
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

	map<string, Var> & Arguments::keywords() {
		return _keywords;
	}

	static string format(Env & env, const string & fmt, vector<Var> & args) {
		string ret;
		size_t f = 0;
		size_t s = 0;
		Iterator<Var> iter(args);
		while ((f = fmt.find("~", f)) != string::npos) {
			if (f - s > 0) {
				ret.append(fmt.substr(s, f - s));
			}
			if (fmt[f + 1] == '%') {
				ret.append("\n");
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'a') {
				ret.append(eval(iter.next(), env).toString());
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'd') {
				string num = eval(iter.next(), env).toString();
				ret.append(num);
				s = f = (f + 2);
			} else if (fmt[f + 1] == ':' && fmt[f + 2] == 'd') {
				string num = eval(iter.next(), env).toString();
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

	bool isNumber(Var var) {
		return var.isInteger() || var.isFloat();
	}

	Var pathname(Var path) {
		if (path.isFile()) {
			return path;
		}
		File file(path.toString());
		return Var(file);
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

	Var toFloat(Var v) {
		if (v.isInteger()) {
			return Var((float)(*v.getInteger()));
		}
		return v;
	}

	bool eq(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return v1.getFloat() == v2.getFloat();
		}
		return v1.getInteger() == v2.getInteger();
	}

	bool gt(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return v1.getFloat() > v2.getFloat();
		}
		return v1.getInteger() > v2.getInteger();
	}

	bool lt(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return v1.getFloat() < v2.getFloat();
		}
		return v1.getInteger() < v2.getInteger();
	}

	bool gteq(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return v1.getFloat() >= v2.getFloat();
		}
		return v1.getInteger() >= v2.getInteger();
	}

	bool lteq(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return v1.getFloat() <= v2.getFloat();
		}
		return v1.getInteger() <= v2.getInteger();
	}

	Var plus(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return Var(v1.getFloat() + v2.getFloat());
		}
		return Var(v1.getInteger() + v2.getInteger());
	}

	Var minus(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return Var(v1.getFloat() - v2.getFloat());
		}
		return Var(v1.getInteger() - v2.getInteger());
	}

	Var multiply(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return Var(v1.getFloat() * v2.getFloat());
		}
		return Var(v1.getInteger() * v2.getInteger());
	}

	Var divide(Var v1, Var v2) {
		if (v1.isFloat() || v2.isFloat()) {
			v1 = toFloat(v1);
			v2 = toFloat(v2);
			return Var(v1.getFloat() / v2.getFloat());
		}
		return Var(v1.getInteger() / v2.getInteger());
	}

	Var function(Var & var, Env & env) {
		if (var.isFunction()) {
			return var;
		}
		if (var.isSymbol()) {
			if (!env[var.getSymbol()].isFunction()) {
				throw LispException("invalid function - '" + var.getSymbol() + "'");
			}
			return env[var.getSymbol()];
		}
		Var func = eval(var, env);
		if (func.isFunction()) {
			return func;
		}
		throw LispException("invalid function - '" + var.toString() + "'");
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

	Var read_from_tokens(vector<string>::iterator & iter, vector<string>::iterator & end) {

		if (iter == end) {
			throw LispException("syntax error - unexpected EOF");
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
			throw LispException("syntax error - unexpected )");
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

	bool silentsymboleq(Var & var, const string & sym) {
		if (var.isSymbol()) {
			return var.getSymbol() == sym;
		}
		return false;
	}

	Var refeval(Var & var, Env & env) {
		if (var.isSymbol()) {
			if (env[var.getSymbol()].isNil() || env[var.getSymbol()].isFunction()) {
				throw LispException("unbound variable - '" + var.getSymbol() + "'");
			}
			PUSH_AND_RETURN(env, Var(&env[var.getSymbol()]));
		}
		return eval(var, env);
	}

	Var eval(Var & var, Env & env) {
	
		if (var.isSymbol()) {
			if (!env.find(var.getSymbol()) || env[var.getSymbol()].isFunction()) {
				throw LispException("unbound variable - '" + var.getSymbol() + "'");
			}
			PUSH_AND_RETURN(env, env[var.getSymbol()]);
		} else if (!var.isList()) {
			PUSH_AND_RETURN(env, var);
		} else if (var.getList().empty()) {
			PUSH_AND_RETURN(env, nil());
		} else {
			vector<Var> & lv = var.getList();
			if (silentsymboleq(lv[0], "quit")) {
				env.quit(true);
			} else if (silentsymboleq(lv[0], "lambda")) {
				testArgumentCount(lv, 3);
				lv[1].checkTypeThrow(Var::LIST);
				Var func(Func(lv[1], lv[2]));
				PUSH_AND_RETURN(env, func);
			} else if (silentsymboleq(lv[0], "defun")) {
				testArgumentCount(lv, 4);
				lv[2].checkTypeThrow(Var::LIST);
				env[lv[1].getSymbol()] = Var(Func(lv[2], lv[3]));
				PUSH_AND_RETURN(env, Var(lv[1].getSymbol()));
			} else if (silentsymboleq(lv[0], "setf")) {
				testArgumentCount(lv, 3);
				refeval(lv[1], env);
				Var var = env.last();
				eval(lv[2], env);
				Var other = env.last();
				var = other;
				PUSH_AND_RETURN(env, var);
			} else if (silentsymboleq(lv[0], "setq")) {
				testArgumentCount(lv, 3);
				Var val = eval(lv[2], env);
				env[lv[1].getSymbol()] = val;
				PUSH_AND_RETURN(env, val);
			} else if (silentsymboleq(lv[0], "quote")) {
				testArgumentCount(lv, 2);
				PUSH_AND_RETURN(env, lv[1]);
				// TODO: implement backquote (quasiquote)
			} else if (silentsymboleq(lv[0], "function")) {
				testArgumentCount(lv, 2);
				Var func = function(lv[1], env);
				PUSH_AND_RETURN(env, func);
			} else if (silentsymboleq(lv[0], "funcall")) {
				testArgumentCount(lv, 2);
				Var funcsym = eval(lv[1], env);
				Var func = function(funcsym, env);
				vector<Var> args(lv.begin() + 2, lv.end());
				Var ret = func.proc(args, env);
				PUSH_AND_RETURN(env, ret);
			} else if (silentsymboleq(lv[0], "let")) {
				testArgumentCount(lv, 2);
				Var ret;
				vector<Var> & lets = lv[1].getList();
				Env e(&env);
				for (vector<Var>::iterator iter = lets.begin(); iter != lets.end(); iter++) {
					vector<Var> decl = (*iter).getList();
					string sym = decl[0].getSymbol();
					e.local()[sym] = eval(decl[1], env);
				}
				for (vector<Var>::iterator iter = lv.begin() + 2; iter != lv.end(); iter++) {
					ret = eval(*iter, e);
				}
				PUSH_AND_RETURN(env, ret);
			} else if (silentsymboleq(lv[0], "if")) {
				testArgumentCount(lv, 3);
				Var val = eval(lv[1], env);
				Var ret;
				if (!val.isNil()) {
					ret = eval(lv[2], env);
				} else if (lv.size() > 3) {
					ret = eval(lv[3], env);
				}
				PUSH_AND_RETURN(env, ret);
			} else if (silentsymboleq(lv[0], "when")) {
				testArgumentCount(lv, 3);
				Var test = eval(lv[1], env);
				if (!test.isNil()) {
					Var ret = eval(lv[2], env);
					PUSH_AND_RETURN(env, ret);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "unless")) {
				testArgumentCount(lv, 3);
				Var test = eval(lv[1], env);
				if (test.isNil()) {
					Var ret = eval(lv[2], env);
					PUSH_AND_RETURN(env, ret);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "cond")) {
				testArgumentCount(lv, 1);
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					vector<Var> lst = iter->getList();
					if (!eval(lst[0], env).isNil()) {
						Var ret = eval(lst[1], env);
						PUSH_AND_RETURN(env, ret);
					}
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "progn")) {
				testArgumentCount(lv, 1);
				Var ret;
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					ret = eval(*iter, env);
				}
				PUSH_AND_RETURN(env, ret);
			} else if (silentsymboleq(lv[0], "while")) {
				testArgumentCount(lv, 3);
				Var pre_test = lv[1];
				while (!eval(pre_test, env).isNil()) {
					eval(lv[2], env);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "dolist")) {
				testArgumentCount(lv, 3);
				Env e(&env);
				vector<Var> decl = lv[1].getList();
				string param = decl[0].getSymbol();
				vector<Var> lst = eval(decl[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
					e.local()[param] = *iter;
					eval(lv[2], e);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "dotimes")) {
				testArgumentCount(lv, 3);
				Env e(&env);
				vector<Var> steps = lv[1].getList();
				string sym = steps[0].getSymbol();
				Integer limit = eval(steps[1], env).getInteger();
				e.local()[sym] = Var(Integer(0));
				for (; e[sym].getInteger() < limit; e[sym] = Var(e[sym].getInteger() + 1)) {
					eval(lv[2], e);
				}
				PUSH_AND_RETURN(env, nil());
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
				testArgumentCount(lv, 1);
				vector<Var> elts;
				for (vector<Var>::iterator iter = lv.begin() + 1; iter != lv.end(); iter++) {
					Var elt = eval(*iter, env);
					elts.push_back(elt);
				}
				PUSH_AND_RETURN(env, Var(elts));
			} else if (silentsymboleq(lv[0], "cons")) {
				testArgumentCount(lv, 3);
				vector<Var> ret;
				Var cons = eval(lv[1], env);
				Var cell = eval(lv[2], env);
				ret.push_back(cons);
				if (cell.isList()) {
					vector<Var> lst = cell.getList();
					ret.insert(ret.end(), lst.begin(), lst.end());
				} else {
					ret.push_back(cell);
				}
				PUSH_AND_RETURN(env, Var(ret));
			} else if (silentsymboleq(lv[0], "car")) {
				testArgumentCount(lv, 2);
				refeval(lv[1], env);
				vector<Var> & lst = (*env.last()).getList();
				if (lst.size() > 0) {
					Var ret(&lst[0]);
					PUSH_AND_RETURN(env, ret);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "cdr")) {
				testArgumentCount(lv, 2);
				refeval(lv[1], env);
				vector<Var> & lst = (*env.last()).getList();
				if (lst.size() > 1) {
					vector<Var> rest;
					for (vector<Var>::iterator iter = lst.begin() + 1; iter != lst.end(); iter++) {
						rest.push_back(Var(&(*iter)));
					}
					Var ret(rest);
					PUSH_AND_RETURN(env, ret);
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "nth")) {
				testArgumentCount(lv, 3);
				size_t idx = (size_t)(*(eval(lv[1], env).getInteger()));
				refeval(lv[2], env);
				vector<Var> & lst = (*env.last()).getList();
				if (idx < lst.size()) {
					PUSH_AND_RETURN(env, Var(&lst[idx]));
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "nthcdr")) {
				testArgumentCount(lv, 3);
				size_t idx = (size_t)(*eval(lv[1], env).getInteger());
				refeval(lv[2], env);
				vector<Var> & lst = (*env.last()).getList();
				if (idx < lst.size()) {
					vector<Var> rest;
					for (vector<Var>::iterator iter = lst.begin() + idx; iter != lst.end(); iter++) {
						rest.push_back(Var(&(*iter)));
					}
					PUSH_AND_RETURN(env, Var(rest));
				}
				PUSH_AND_RETURN(env, nil());
			} else if (silentsymboleq(lv[0], "subseq")) {
				testArgumentCount(lv, 4);
				refeval(lv[1], env);
				vector<Var> & lst = (*env.last()).getList();
				Integer start = eval(lv[2], env).getInteger();
				Integer end = eval(lv[3], env).getInteger();
				vector<Var> ret;
				for (size_t i = (size_t)*start; i < (size_t)*end && i < lst.size(); i++) {
					ret.push_back(Var(&lst[i]));
				}
				PUSH_AND_RETURN(env, Var(ret));
			} else if (silentsymboleq(lv[0], "defmacro")) {
				
				throw LispException("not implemeneted");
				
				// TODO: implement
				// refer [http://clhs.lisp.se/Body/m_defmac.htm]
				
			} else if (silentsymboleq(lv[0], "macroexpand")) {
				
				throw LispException("not implemeneted");
				
				// TODO: implement
				
			} else {
				vector<Var> args(lv.begin() + 1, lv.end());
				Var func = function(lv[0], env);
				Var ret = func.proc(lv[0], args, env);
				PUSH_AND_RETURN(env, ret);
			}
		}

		PUSH_AND_RETURN(env, nil());
	}

	Var compile(const string & cmd, Env & env) {
		env.stack().clear();
		Var tokens = parse(BufferedCommandReader::trimComment(cmd));
		eval(tokens, env);
		Var ret;
		ret = *env.last();
		env.stack().clear();
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
				testArgumentCount(args, 1);

				// Obj<Var> var = eval(args[0], env);
				// return env.alloc(new Var(var.isSymbol()));
				
				Var var = eval(args[0], env);
				return Var(var.isSymbol());
			});
		DECL_NATIVE("listp", Listp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isList());
			});
		DECL_NATIVE("booleanp", Booleanp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isBoolean());
			});
		DECL_NATIVE("integerp", Integerp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isInteger());
			});
		DECL_NATIVE("floatp", Floatp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isFloat());
			});
		DECL_NATIVE("stringp", Stringp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isString());
			});
		DECL_NATIVE("funcp", Funcp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isFunction());
			});
		DECL_NATIVE("pathnamep", Pathnamep, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isFile());
			});
		DECL_NATIVE("streamp", Streamp, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isFileDescriptor());
			});
	}

	void builtin_algorithm(Env & env) {

		// remove

		// TODO: refer - [http://www.lispworks.com/documentation/lw60/CLHS/Body/f_map.htm]
		DECL_NATIVE("map", Map, {
				testArgumentCount(args, 3);
				Var sym = eval(args[0], env); /* TODO: use it */
				Var func = eval(args[1], env);
				func = function(func, env);
				Var seq = eval(args[2], env); /* TODO: use it */

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

				return Var(ret);
			});

		DECL_NATIVE("sort", Sort, {
				testArgumentCount(args, 2);
				vector<Var> lst = eval(args[0], env).getList();
				Var func = eval(args[1], env);
				func = function(func, env);

				if (lst.size() <= 1) {
					return Var(lst);
				}

				for (size_t loop = 0; loop < lst.size() - 1; loop++) {
					for (size_t i = 0; i < lst.size() - 1; i++) {
						vector<Var> fargs;
						fargs.push_back(lst[i]);
						fargs.push_back(lst[i + 1]);
						if (!func.proc(Var("#sort"), fargs, env).isNil()) {
							iter_swap(lst.begin() + i, lst.begin() + (i + 1));
						}
					}
				}
				return Var(lst);
			});
	}

	void builtin_list(Env & env) {
		DECL_NATIVE("length", Length, {
				testArgumentCount(args, 1);
				vector<Var> lst = eval(args[0], env).getList();
				return Var(Integer((long long)lst.size()));
			});
		
		DECL_NATIVE("append", Append, {
				testArgumentCount(args, 0);
				vector<Var> ret;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					vector<Var> lst = eval(*iter, env).getList();
					ret.insert(ret.end(), lst.begin(), lst.end());
				}
				return Var(ret);
			});
		
		DECL_NATIVE("remove", Remove, {
				testArgumentCount(args, 2);
				Var val = eval(args[0], env);
				vector<Var> lst = eval(args[1], env).getList();
				for (vector<Var>::iterator iter = lst.begin(); iter != lst.end();) {
					if (val.toString() == iter->toString()) {
						iter = lst.erase(iter);
					} else {
						iter++;
					}
				}
				return Var(lst);
			});
        
        class RemoveIf : public Procedure {
        private:
        public:
            RemoveIf(const string & name) : Procedure(name) {}
            virtual ~RemoveIf() {}
            virtual Var proc(Var name, vector<Var> & args, Env & env) {
                testArgumentCount(args, 2);
                Var func = eval(args[0], env);
				func = function(func, env);
                vector<Var> lst = eval(args[1], env).getList();
                for (vector<Var>::iterator iter = lst.begin(); iter != lst.end();) {
                    vector<Var> fargs;
                    fargs.push_back(*iter);
                    if (!func.proc(fargs, env).isNil()) {
                        iter = lst.erase(iter);
                    } else {
                        iter++;
                    }
                }
                return Var(lst);
            }
        };
        env["remove-if"] = Var(AutoRef<Procedure>(new RemoveIf("remove-if")));

		// TODO: implement
		// [https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html]
		// remove-if-not
		// [http://www.jtra.cz/stuff/lisp/sclr/push.html]
		// push
		// pop
	}

	void builtin_logic(Env & env) {
		DECL_NATIVE("not", Not, {
				testArgumentCount(args, 1);
				Var var = eval(args[0], env);
				return Var(var.isNil());
			});

		DECL_NATIVE("or", Or, {
				testArgumentCount(args, 0);
				Var var;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (!var.isNil()) {
						break;
					}
				}
				return var;
			});

		DECL_NATIVE("and", And, {
				testArgumentCount(args, 0);
				Var var("t");
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					var = eval(*iter, env);
					if (var.isNil()) {
						break;
					}
				}
				return var;
			});
	}

	void builtin_string(Env & env) {

		DECL_NATIVE("string=", LiteralEqual, {
				testArgumentCount(args, 1);
				string val = eval(args[0], env).toString();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (val != eval(*iter, env).toString()) {
						return nil();
					}
				}
				return Var(true);
			});

		DECL_NATIVE("string-prefix-p", StringPrefixP, {
				testArgumentCount(args, 2);
				string str = eval(args[0], env).toString();
				string dst = eval(args[1], env).toString();
				return Var(Text::startsWith(str, dst));
			});

		DECL_NATIVE("string-suffix-p", StringSuffixP, {
				testArgumentCount(args, 2);
				string str = eval(args[0], env).toString();
				string dst = eval(args[1], env).toString();
				return Var(Text::endsWith(str, dst));
			});

		DECL_NATIVE("string-length", StringLength, {
				testArgumentCount(args, 1);
				Integer len((long long)eval(args[0], env).toString().length());
				return Var(len);
			});

		DECL_NATIVE("string-append", StringAppend, {
				testArgumentCount(args, 0);
				string ret;
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					ret.append(eval(*iter, env).toString());
				}
				return Var(text(ret));
			});

		DECL_NATIVE("format", Format, {
				testArgumentCount(args, 2);
				Var test = eval(args[0], env);
				vector<Var> fargs(args.begin() + 2, args.end());
				string str = format(env, args[1].toString(), fargs);
				if (!test.isNil()) {
					fputs(str.c_str(), stdout);
					fputs("\n", stdout);
					return nil();
				} else {
					return Var(text(str));
				}
			});
		
		DECL_NATIVE("enough-namestring", EnoughNamestring, {
				testArgumentCount(args, 2);
				string org = eval(args[0], env).toString();
				string prefix = eval(args[1], env).toString();
				if (Text::startsWith(org, prefix)) {
					return Var(text(org.substr(prefix.length())));
				}
				return Var(text(org));
			});
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE("=", ArithmeticEqual, {
				testArgumentCount(args, 1);
				Var v = eval(args[0], env);
				Integer val = v.getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					if (!eq(v, eval(*iter, env))) {
						return nil();
					}
				}
				return Var(true);
			});
		DECL_NATIVE("+", Plus, {
				Var v(0);
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					v = plus(v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("-", Minus, {
				testArgumentCount(args, 1);
				Var v(0);
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					v = minus(v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("*", Multitude, {
				Var v(1);
				for (vector<Var>::iterator iter = args.begin(); iter != args.end(); iter++) {
					v = multiply(v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("/", Divide, {
				testArgumentCount(args, 1);
				Var v = eval(args[0], env);
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					v = divide(v, eval(*iter, env));
				}
				return v;
			});
		DECL_NATIVE("%", Rest, {
				testArgumentCount(args, 1);
				Integer sum = eval(args[0], env).getInteger();
				for (vector<Var>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					sum %= eval(*iter, env).getInteger();
				}
				return Var(sum);
			});

		DECL_NATIVE(">", Greater, {
				testArgumentCount(args, 2);
				return Var(gt(eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE("<", Less, {
				testArgumentCount(args, 2);
				return Var(lt(eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE(">=", GreaterEq, {
				testArgumentCount(args, 2);
				return Var(gteq(eval(args[0], env), eval(args[1], env)));
			});

		DECL_NATIVE("<=", LessEq, {
				testArgumentCount(args, 2);
				return Var(lteq(eval(args[0], env), eval(args[1], env)));
			});
	}
	void builtin_io(Env & env) {

		env["*standard-output*"] = Var(FileDescriptor(stdout));
		env["*standard-input*"] = Var(FileDescriptor(stdin));
		
        class Read : public Procedure {
        public:
            Read(const string & name) : Procedure(name) {}
            virtual ~Read() {}
            virtual Var proc(Var name, vector<Var> & args, Env & env) {
                testArgumentCount(args, 1);
                Var ret;
                FileDescriptor fd = eval(args[0], env).getFileDescriptor();
                if (fd.eof()) {
                    return Var(true);
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
        env["read"] = Var(AutoRef<Procedure>(new Read("read")));

		DECL_NATIVE("read-line", ReadLine, {
				testArgumentCount(args, 1);
				FileDescriptor fd = eval(args[0], env).getFileDescriptor();
				if (fd.eof()) {
					return Var(true);
				}
				string line = fd.readline();
				return Var(text(line));
			});
		DECL_NATIVE("print", Print, {
				testArgumentCount(args, 1);
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				fd.write("\n");
				return Var(text(msg));
			});

		DECL_NATIVE("write-string", WriteString, {
				testArgumentCount(args, 1);
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				return Var(text(msg));
			});

		DECL_NATIVE("write-line", WriteLine, {
				testArgumentCount(args, 1);
				FileDescriptor fd = env["*standard-output*"].getFileDescriptor();
				if (args.size() == 2) {
					fd = eval(args[1], env).getFileDescriptor();
				}
				string msg = eval(args[0], env).toString();
				fd.write(msg);
				fd.write("\n");
				return Var(text(msg));
			});
	}
	void builtin_pathname(Env & env) {
		DECL_NATIVE("pathname", Pathname, {
				testArgumentCount(args, 1);
				Var path = pathname(eval(args[0], env));
				return path;
			});
		DECL_NATIVE("pathname-name", PathnameName, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(text(file.getFileNameWithoutExtension()));
			});
		DECL_NATIVE("pathname-type", PathnameType, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(text(file.getExtension()));
			});
		DECL_NATIVE("namestring", Namestring, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(text(file.getPath()));
			});
		DECL_NATIVE("directory-namestring", DirectoryNamestring, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(text(file.getDirectory()));
			});
		DECL_NATIVE("file-namestring", FileNamestring, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(text(file.getFileName()));
			});
	}
	void builtin_file(Env & env) {
		DECL_NATIVE("dir", Dir, {
				testArgumentCount(args, 0);
				Var path = ((args.size() > 0) ? pathname(eval(args[0], env)) : Var("#p\".\""));
				vector<File> files = File::list(path.getFile().getPath());
				vector<Var> lst;
				for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
					lst.push_back(Var(*iter));
				}
				return Var(lst);
			});
		DECL_NATIVE("probe-file", ProbeFile, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return file.exists() ? Var(file) : nil();
			});
		DECL_NATIVE("dirp", Dirp, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(file.isDirectory());
			});
		DECL_NATIVE("filep", Filep, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(file.isFile());
			});
		DECL_NATIVE("file-length", FileLength, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				return Var(Integer((long long)file.getSize()));
			});


		class Open : public Procedure {
		public:
			Open(const string & name) : Procedure(name) {}
			virtual ~Open() {}
			virtual Var proc(Var name, vector<Var> & args, Env & env) {
				map<string, Var> keywords = Arguments::extractKeywords(args);
				File file = pathname(eval(args[0], env)).getFile();
				const char * flags = "rb+";
				if (!file.exists()) {
					// does not exists
					if (HAS(keywords, ":if-does-not-exist")) {
						if (keywords[":if-does-not-exist"].isNil()) {
							return nil();
						} else if (keywords[":if-does-not-exist"].getSymbol() == ":create") {
							flags = "wb+";
						}
					}
				} else {
					// exists
					if (HAS(keywords, ":if-exists")) {
						if (keywords[":if-exists"].isNil()) {
							return nil();
						} else if (keywords[":if-exists"].getSymbol() == ":append") {
							flags = "ab+";
						} else if (keywords[":if-exists"].getSymbol() == ":overwrite") {
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
				return Var(FileDescriptor(fp));
#elif defined(USE_MS_WIN)
				FILE * fp = NULL;
				if (fopen_s(&fp, file.getPath().c_str(), flags) != 0) {
					throw LispException("Cannot open file");
				}
				return Var(FileDescriptor(fp));
#endif		
			}
		};
		env["open"] = Var(AutoRef<Procedure>(new Open("open")));

		DECL_NATIVE("file-position", FilePosition, {
				testArgumentCount(args, 1);
				FileDescriptor fd = eval(args[0], env).getFileDescriptor();
				if (args.size() > 1) {
					fd.position((size_t)*eval(args[1], env).getInteger());
				}
				return Var(Integer((long long)fd.position()));
				
			});
		
		DECL_NATIVE("close", Close, {
				testArgumentCount(args, 1);
				eval(args[0], env).getFileDescriptor().close();
				return nil();
			});
	}
	void builtin_socket(Env & env) {
		// TODO: implement
	}
	void builtin_system(Env & env) {
		DECL_NATIVE("system", System, {
				testArgumentCount(args, 1);
				Integer ret(system(eval(args[0], env).toString().c_str()));
				return Var(ret);
			});
		DECL_NATIVE("load", Load, {
				testArgumentCount(args, 1);
				File file = pathname(eval(args[0], env)).getFile();
				FileReader fileReader(file);
				string dump = fileReader.dumpAsString();
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
				return Var(true);
			});
	}
	void builtin_date(Env & env) {
		env["internal-time-units-per-second"] = Var(1000);
		DECL_NATIVE("now", Now, {
				testArgumentCount(args, 0);
				char buffer[512];
				memset(buffer, 0, sizeof(buffer));
				Date date = Date::now();
				snprintf(buffer, sizeof(buffer), "%04d-%02d-%02d %02d:%02d:%02d.%d",
						 date.getYear(), date.getMonth() + 1, date.getDay(),
						 date.getHour(), date.getMinute(), date.getSecond(),
						 date.getMillisecond());
				return Var(text(buffer));
			});
		DECL_NATIVE("get-universal-time", GetUniversalTime, {
				testArgumentCount(args, 0);
				return Var((long long)osl_get_time_network().sec);
			});
		DECL_NATIVE("decode-universal-time", DecodeUniversalTime, {
				testArgumentCount(args, 1);
				// TODO: implement
				throw LispException("not implemented");
			});
		DECL_NATIVE("encode-universal-time", EncodeUniversalTime, {
				testArgumentCount(args, 6); // seconds, minutes, hours, dates, month and year (gmt offset)
				// TODO: implement
				throw LispException("not implemented");
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

	string printVar(const Var & var) {
		if (var.isString()) {
			return var.getString();
		}
		return var.toString();
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
