#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "Iterator.hpp"
#include "FileStream.hpp"

#define _CONTAINS(M,E) (M.find(E) != M.end())
#define _HEAP_ALLOC(E,V) E.alloc(new Var(V))
#define _VAR GCRef<Var>
#define _NIL(e) _HEAP_ALLOC(e,"nil")
#define _CHECK_ARGS_MIN_COUNT(L,C) validateArgumentCountMin(L,C)
#define _CHECK_ARGS_EXACT_COUNT(L,C) validateArgumentCountExact(L,C)
#define _EQ_NIL_OR_SYMBOL(A,B) (((A)->isNil() && (B)->isNil()) ||		\
								(((A)->isNil() == false && (B)->isNil() == false) && \
								 ((A)->r_symbol() == (B)->r_symbol())))
#define _OPT_EVAL(E,S,L,N,D) (N < L.size() ? eval(E, S, L[N]) : D)
#define _FORI(L,I,F) for (size_t I = F; I < L.size(); I++)

#define DECL_NATIVE_BEGIN(ENV,NAME)								\
	do {														\
	Env & _E = ENV;												\
	string _N = NAME;											\
	class _cls : public Procedure {								\
	private:														\
	public:															\
	_cls(const string & name) : Procedure(name) {}					\
	virtual ~_cls() {}												\
	virtual _VAR proc(Env & env, Scope & scope, _VAR name, vector<_VAR> & args) {
#define DECL_NATIVE_END()											\
	}																\
	};																\
	_E.scope().put(_N, _E.alloc(new Var(AutoRef<Procedure>(new _cls(_N))))); \
	} while (0);

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static string _to_string(_VAR var) {
		if (var.nil()) {
			return "(undefined)";
		}
		return var->toString();
	}

	ExitLispException::ExitLispException() : _code(0) {
	}
	ExitLispException::ExitLispException(int code) : _code(code) {
	}
	ExitLispException::~ExitLispException() throw() {
	}
	int & ExitLispException::code() {
		return _code;
	}

	/**
	 * @brief return lisp exception
	 */
	
	ReturnLispException::ReturnLispException(_VAR tag, _VAR var)
		: _tag(tag), _var(var) {
	}
	ReturnLispException::~ReturnLispException() throw() {
	}
	_VAR ReturnLispException::tag() {
		return _tag;
	}
	_VAR ReturnLispException::var() {
		return _var;
	}

	/**
	 * @brief throw lisp exception
	 */
	
	ThrowLispException::ThrowLispException(_VAR except, _VAR ret)
		: _except(except), _ret(ret) {
	}
	ThrowLispException::~ThrowLispException() throw() {
	}
	_VAR ThrowLispException::except() {
		return _except;
	}
	_VAR ThrowLispException::ret() {
		return _ret;
	}

	/**
	 * @brief scope
	 */

	Scope::Scope() {
	}
	Scope::~Scope() {
	}
	Ref<Scope> & Scope::parent() {
		return _parent;
	}
	void Scope::clear() {
		_registry.clear();
	}
	_VAR Scope::rsearch(const string & name) {
		if (_registry.find(name) != _registry.end()) {
			return _registry[name];
		}
		if (_parent.nil() == false) {
			return _parent->rsearch(name);
		}
		return _VAR();
	}
	_VAR & Scope::rget(const std::string & name) {
		if (_registry.find(name) != _registry.end()) {
			return _registry[name];
		}
		if (_parent.nil() == false) {
			return _parent->rget(name);
		}
		return _registry[name];
	}
	map<string, _VAR > & Scope::registry() {
		return _registry;
	}
	_VAR Scope::get(const string & name) {
		if (_registry.find(name) == _registry.end()) {
			throw LispException("unbound variable - '" + name + "'");
		}
		return _registry[name];
	}
	void Scope::put(const string & name, const _VAR & var) {
		_registry[name] = var;
	}
	int Scope::depth() {
		if (_parent.nil() == false) {
			return _parent->depth() + 1;
		}
		return 1;
	}
	string Scope::toString() const {
		string indent;
		string ret;
		if (_parent.nil() == false) {
			ret.append(_parent->toString());
			ret.append("\n");
			indent = "    ";
		}
		ret.append(indent);
		ret.append("[");
		for (map<string, _VAR>::const_iterator iter = _registry.begin(); iter != _registry.end(); iter++) {
			if (iter != _registry.begin()) {
				ret.append("\n");
			}
			ret.append(indent);
			ret.append("'");
			ret.append(iter->first);
			ret.append("': '");
			ret.append(_to_string(iter->second));
			ret.append("'");
		}
		ret.append(indent);
		ret.append("]");
		return ret;
	}

	/**
	 * procedure
	 */

	Procedure::Procedure(const std::string & name) : _name(name) {
	}
	Procedure::~Procedure() {
	}
	Scope & Procedure::scope() {
		return _scope;
	}
	std::string & Procedure::name() {
		return _name;
	}
	_VAR & Procedure::doc() {
		return _doc;
	}
	
	/**
	 * @brief Env
	 */

	bool Env::_debug = false;

	Env::Env() {
		_trace("init");
	}
	Env::~Env() {
		clear();
		_trace("deinit");
	}
	void Env::setDebug(bool debug) {
		_debug = debug;
	}
	void Env::_trace(const string & msg) {
		if (_debug) {
			printf(" ## Env::trace / %p -- %s\n", this, msg.c_str());
		}
	}

	Scope & Env::scope() {
		return _scope;
	}
	SharedHeap<Var> & Env::heap() {
		return _heap;
	}
	_VAR Env::alloc(Var * var) {
		AutoLock lock(heap().sem());
		return heap().alloc(var);
	}
	void Env::gc() {
		AutoLock lock(_heap.sem());
		size_t size = _heap.size();
		unsigned long elapsed = _heap.gc();
		if (_debug) {
			printf(" # GC / %d, dealloc: %d (%ld ms.) #\n", (int)_heap.size(), (int)(size - _heap.size()), elapsed);
		}
	}
	void Env::clear() {
		_scope.clear();
		gc();
		AutoLock lock(_heap.sem());
		_heap.clear();
	}

	/**
	 * @brief Func
	 */

	Func::Func() {
	}
	Func::Func(const _VAR & params, const _VAR & body) {
		_params = params;
		_body = body;
	}
	Func::~Func() {
	}
	Scope & Func::scope() {
		return _scope;
	}
	_VAR & Func::doc() {
		return _doc;
	}
	_VAR & Func::params() {
		return _params;
	}
	_VAR & Func::body() {
		return _body;
	}
	const _VAR Func::doc() const {
		return _doc;
	}
	const _VAR Func::params() const {
		return _params;
	}
	const _VAR Func::body() const {
		return _body;
	}
	bool Func::empty() {
		return _params.nil() || _body.nil() || _params->isNil() || _body->isNil();
	}

	/**
	 * @brief Var
	 */

	bool Var::_debug = false;

	Var::Var() : _type(NIL) {
		_trace("init - NIL");
	}
	Var::Var(const char * token) : _type(NIL) {
		init(string(token));
		_trace("init - const char *");
	}
	Var::Var(const string & token) : _type(NIL) {
		init(token);
		_trace("init - string");
	}
	Var::Var(vector<_VAR> lst) : _type(LIST), _lst(lst) {
		_trace("init - list");
	}
	Var::Var(bool bval) : _type(BOOLEAN), _bval(bval) {
		if (!bval) {
			_type = NIL;
		}
		_trace("init - bool");
	}
	Var::Var(const Boolean & bval) : _type(BOOLEAN), _bval(bval) {
		if (!bval.val()) {
			_type = NIL;
		}
		_trace("init - Boolean");
	}
	Var::Var(short inum) : _type(INTEGER), _inum(inum) {
		_trace("init - short");
	}
	Var::Var(int inum) : _type(INTEGER), _inum(inum) {
		_trace("init - int");
	}
	Var::Var(long inum) : _type(INTEGER), _inum(inum) {
		_trace("init - long");
	}
	Var::Var(long long inum) : _type(INTEGER), _inum(inum) {
		_trace("init - long long");
	}
	Var::Var(const Integer & inum) : _type(INTEGER), _inum(inum) {
		_trace("init - Integer");
	}
	Var::Var(float fnum) : _type(FLOAT), _fnum(fnum) {
		_trace("init - float");
	}
	Var::Var(double fnum) : _type(FLOAT), _fnum(fnum) {
		_trace("init - double");
	}
	Var::Var(const Float & fnum) : _type(FLOAT), _fnum(fnum) {
		_trace("init - Float");
	}
	Var::Var(const Func & func) : _type(FUNC), _func(func) {
		_trace("init - Func");
	}
	Var::Var(AutoRef<Procedure> procedure) : _type(FUNC), _procedure(procedure) {
		_trace("init - Procedure");
	}
	Var::Var(File & file) : _type(FILE), _file(file) {
		_trace("init - File");
	}
	Var::Var(const FileDescriptor & fd) : _type(FILE_DESCRIPTOR), _fd(fd) {
		_trace("init - FileDescriptor");
	}
	Var::~Var() {
		_trace("deinit");
	}

	void Var::_trace(const string & msg) {
		if (_debug) {
			printf(" ## Var::trace / %p -- %s\n", this, msg.c_str());
		}
	}

	void Var::setDebug(bool debug) {
		_debug = debug;
	}

	void Var::init(const string & token) {
		if (token.empty()) {
			throw LispException("empty token");
		}
		if (token == "nil") {
			_type = NIL;
		} else if (token == "t") {
			_type = BOOLEAN;
			_bval = true;
		} else if (*token.begin() == '\"' && *token.rbegin() == '\"') {
			_type = STRING;
			_str = token;
		} else if (Integer::isIntegerString(token)) {
			_type = INTEGER;
			_inum = Integer::toInteger(token);
		} else if (Float::isFloatString(token)) {
			_type = FLOAT;
			_fnum = Float::toFloat(token);
		} else if (*token.begin() == '#' && *(token.begin() + 1) == 'p') {
			_type = FILE;
			_file = File(token.substr(3, token.length() - 4));
		} else {
			_type = SYMBOL;
			_symbol = token;
		}
	}
	int Var::getType() { return _type; }
	string Var::getTypeString() const {
		return getTypeString(_type);
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
		throw LispException("unknown variable type / " + Text::toString(_type));
	}
	void Var::typeCheck(int t) const {
		if (_type != t) {
			throw LispException("Type check failed (required: " + getTypeString(t) +
								", but: " + getTypeString() + ")");
		}
	}
	bool Var::isNil() const {return _type == NIL;}
	bool Var::isList() const {return _type == LIST;}
	bool Var::isSymbol() const {return _type == SYMBOL;}
	bool Var::isBoolean() const {return _type == BOOLEAN;}
	bool Var::isInteger() const {return _type == INTEGER;}
	bool Var::isFloat() const {return _type == FLOAT;}
	bool Var::isString() const {return _type == STRING;}
	bool Var::isFunction() const {return _type == FUNC;}
	bool Var::isFile() const {return _type == FILE;}
	bool Var::isFileDescriptor() const {return _type == FILE_DESCRIPTOR;}
	string & Var::r_symbol() {typeCheck(SYMBOL); return _symbol;}
	string & Var::r_string() {typeCheck(STRING); return _str;}
	vector<_VAR> & Var::r_list() {typeCheck(LIST); return _lst;}
	Boolean & Var::r_boolean() {typeCheck(BOOLEAN); return _bval;}
	Integer & Var::r_integer() {typeCheck(INTEGER); return _inum;}
	Float & Var::r_float() {typeCheck(FLOAT); return _fnum;}
	File & Var::r_file() {typeCheck(FILE); return _file;}
	Func & Var::r_func() {typeCheck(FUNC); return _func;}
	AutoRef<Procedure> & Var::r_procedure() {typeCheck(FUNC); return _procedure;}
	FileDescriptor & Var::r_fileDescriptor() {typeCheck(FILE_DESCRIPTOR); return _fd;}
	_VAR Var::proc(Env & env, Scope & scope, _VAR name, vector<_VAR> & args) {
		if (!isFunction()) {
			throw LispException("not function / name: '" + name->toString() + "' / type : '" + getTypeString() + "'");
		}
		if (!_procedure.nil()) {
			return _procedure->proc(env, scope, name, args);
		}
		Arguments binder(_func.params()->r_list());
		_func.scope().parent() = Ref<Scope>(&scope);
		binder.mapArguments(env, _func.scope(), scope, args);
		return eval(env, _func.scope(), _func.body());
	}
	_VAR Var::proc(Env & env, Scope & scope, vector<_VAR> & args) {
		if (!_procedure.nil()) {
			return proc(env, scope, _HEAP_ALLOC(env, _procedure->name()), args);
		} else {
			return proc(env, scope, _NIL(env), args);
		}
	}
	string Var::toString() const {
		switch (_type) {
		case NIL:
			return "NIL";
		case SYMBOL:
			return _symbol;
		case LIST:
		{
			if ((_lst.size() > 1) && _lst[0]->isSymbol()) {
				if (_lst[0]->r_symbol() == "quote") {
					return "'" + _lst[1]->toString();
				} else if (_lst[0]->r_symbol() == "`") {
					return "`" + _lst[1]->toString();
				} else if (_lst[0]->r_symbol() == ",") {
					return "," + _lst[1]->toString();
				}
			}
			string ret = "(";
			for (vector<_VAR>::const_iterator iter = _lst.begin(); iter != _lst.end(); iter++) {
				if (iter != _lst.begin()) {
					ret += " ";
				}
				ret += (*iter)->toString();
			}
			ret += ")";
			return ret;
		}
		case BOOLEAN:
			return _bval.toString();
		case INTEGER:
		{
			return Text::toString(_inum.raw());
		}
		case FLOAT:
		{
			return Text::rtrim(Text::toString(_fnum.raw()), "0");
		}
		case STRING:
			return unwrap_text(_str);
		case FUNC:
		{
			if (!_procedure.nil()) {
				return "#<COMPILED FUNCTION " + _procedure->name() + ">";
			}
			return "#<FUNCTION (PARAMS:" + _func.params()->toString() +
				", BODY:" + _func.body()->toString() + ")>";
		}
		case FILE:
			return "#p\"" + _file.getPath() + "\"";
		case FILE_DESCRIPTOR:
			return "#<FD>";
		default:
			break;
		}
		throw LispException("unknown variable type / " + Text::toString(_type));
	}

	/**
	 * @brief Arguments
	 */
	
	static void validateArgumentCountMin(vector<_VAR> & args, size_t expect) {
		if (args.size() < expect) {
			throw LispException("Wrong argument count: " + Text::toString(args.size())
								+ " / expected minimum: " + Text::toString(expect));
		}
	}

	static void validateArgumentCountExact(vector<_VAR> & args, size_t expect) {
		if (args.size() != expect) {
			throw LispException("Wrong argument count: " + Text::toString(args.size())
								+ " / expected: " + Text::toString(expect));
		}
	}
	
	Arguments::Arguments() {}
	Arguments::Arguments(vector<_VAR> & proto) : proto(proto) {}
	Arguments::~Arguments() {}

	size_t Arguments::countPartArguments(vector<_VAR> & arr, size_t start) {
		size_t cnt = start;
		for (; cnt < arr.size(); cnt++) {
			if (Text::startsWith(arr[cnt]->r_symbol(), "&")) {
				break;
			}
		}
		return cnt;
	}
	void Arguments::mapArguments(Env & env,
								 Scope & sub_scope,
								 Scope & scope,
								 vector< _VAR > & args) {
		size_t ec = countPartArguments(proto, 0);
		_CHECK_ARGS_MIN_COUNT(args, ec);
		size_t ai = 0;
		size_t i = 0;
		for (; i < ec; i++, ai++) {
			_VAR val = eval(env, scope, args[ai]);
			sub_scope.put(proto[i]->r_symbol(), val);
		}
		if (i >= proto.size()) {
			return;
		}
		if (proto[i]->r_symbol() == "&optional") {
			size_t offset = mapOptionals(env, sub_scope, scope, proto, ++i, args, ai);
			i += offset;
			ai += offset;
		}
		if (i >= proto.size()) {
			return;
		}
		if (proto[i]->r_symbol() == "&rest") {
			if (i + 1 >= proto.size()) {
				throw LispException("Rest arguments are needed after &rest");
			}
			_VAR val = _HEAP_ALLOC(env, extractRest(env, scope, args, ai));
			sub_scope.put(proto[i + 1]->r_symbol(), val);
		}
		_keywords = extractKeywords(args);
	}
	size_t Arguments::mapOptionals(Env & env,
								   Scope & sub_scope,
								   Scope & scope,
								   vector<_VAR> & proto,
								   size_t pstart,
								   vector<_VAR> & args,
								   size_t astart)
	{
		size_t i = pstart;
		size_t j = astart;
		for (; i < proto.size(); i++, j++) {
			if (proto[i]->isSymbol() && Text::startsWith(proto[i]->r_symbol(), "&")) {
				break;
			}
			string sym;
			if (proto[i]->isSymbol()) {
				sym = proto[i]->r_symbol();
				sub_scope.put(sym, _NIL(env));
			} else if (proto[i]->isList()) {
				_CHECK_ARGS_MIN_COUNT(proto[i]->r_list(), 2);
				sym = proto[i]->r_list()[0]->r_symbol();
				sub_scope.put(sym, proto[i]->r_list()[1]);
			}
			if (j < args.size()) {
				_VAR val = eval(env, scope, args[j]);
				sub_scope.put(sym, val);
			}
		}
		return i - pstart;
	}
	vector<_VAR> Arguments::extractRest(Env & env, Scope & scope, vector<_VAR> & args, size_t start) {
		vector<_VAR> rest;
		for (size_t i = start; i < args.size(); i++) {
			rest.push_back(eval(env, scope, args[i]));
		}
		return rest;
	}
	map<string, _VAR> Arguments::extractKeywords(vector<_VAR> & args) {
		map<string, _VAR> keywords;
		for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
			if ((*iter)->isSymbol() && Text::startsWith((*iter)->r_symbol(), ":")) {
				string name = (*iter)->r_symbol();
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

	// built-in
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
	
	static string format(Env & env, Scope & scope, const string & fmt, vector<_VAR> & args) {
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
				ret.append(eval(env, scope, iter.next())->toString());
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'd') {
				string num = eval(env, scope, iter.next())->toString();
				ret.append(num);
				s = f = (f + 2);
			} else if (fmt[f + 1] == ':' && fmt[f + 2] == 'd') {
				string num = eval(env, scope, iter.next())->toString();
				ret.append(Text::toCommaNumber(num));
				s = f = (f + 3);
			} else if (fmt[f + 1] == '$') {
				_VAR var = eval(env, scope, iter.next());
				string num;
				if (var->isInteger()) {
					num = var->toString();
				} else if (var->isFloat()) {
					num = Text::format("%.2lf", *var->r_float());
				} else {
					throw LispException("wrong format '~$' - '" + var->toString() + "'");
				}
				ret.append(num);
				s = f = (f + 2);
			} else {
				s = f = (f + 1);
			}
		}
		if (s < fmt.length()) {
			ret.append(fmt.substr(s));
		}
		return ret;
	}

	_VAR pathname(Env & env, _VAR path) {
		if (path->isFile()) {
			return path;
		}
		File file(path->toString());
		return _HEAP_ALLOC(env, file);
	}

	string wrap_text(const string & txt) {
		return "\"" + txt + "\"";
	}
	
	string unwrap_text(const string & txt) {
		return txt.substr(1, txt.length() - 2);
	}

	vector<_VAR> listy(_VAR var) {
		if (var->isList()) {
			return var->r_list();
		}
		vector<_VAR> ret;
		ret.push_back(var);
		return ret;
	}

	_VAR toFloat(Env & env, _VAR v) {
		if (v->isInteger()) {
			return _HEAP_ALLOC(env, (double)(*v->r_integer()));
		}
		return v;
	}

	bool eq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->r_float() == v2->r_float();
		}
		return v1->r_integer() == v2->r_integer();
	}

	bool gt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->r_float() > v2->r_float();
		}
		return v1->r_integer() > v2->r_integer();
	}

	bool lt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->r_float() < v2->r_float();
		}
		return v1->r_integer() < v2->r_integer();
	}

	bool gteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->r_float() >= v2->r_float();
		}
		return v1->r_integer() >= v2->r_integer();
	}

	bool lteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return v1->r_float() <= v2->r_float();
		}
		return v1->r_integer() <= v2->r_integer();
	}

	_VAR plus(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return _HEAP_ALLOC(env, v1->r_float() + v2->r_float());
		}
		return _HEAP_ALLOC(env, v1->r_integer() + v2->r_integer());
	}

	_VAR minus(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return _HEAP_ALLOC(env, v1->r_float() - v2->r_float());
		}
		return _HEAP_ALLOC(env, v1->r_integer() - v2->r_integer());
	}

	_VAR multiply(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return _HEAP_ALLOC(env, v1->r_float() * v2->r_float());
		}
		return _HEAP_ALLOC(env, v1->r_integer() * v2->r_integer());
	}

	_VAR divide(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			return _HEAP_ALLOC(env, v1->r_float() / v2->r_float());
		}
		return _HEAP_ALLOC(env, v1->r_integer() / v2->r_integer());
	}

	static _VAR function(Env & env, Scope & scope, const _VAR & var) {
		if (var->isFunction()) {
			return var;
		}
		if (var->isSymbol()) {
			_VAR v = scope.rsearch(var->r_symbol());
			if (v->isFunction() == false) {
				throw LispException("invalid function - '" + v->toString() + "' / type: " + v->getTypeString());
			}
			return v;
		}
		_VAR func = eval(env, scope, var);
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
			} else if (*iter == '\'' || *iter == ',' || *iter == '`') {
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

	static _VAR read_from_tokens(Env & env, vector<string>::iterator & iter, vector<string>::iterator & end) {
		if (iter == end) {
			throw ParseLispException("syntax error - unexpected EOF");
		}
		if (*iter == "(") {
			vector<_VAR> lst;
			iter++;
			for (;*iter != ")"; iter++) {
				_VAR var = read_from_tokens(env, iter, end);
				lst.push_back(var);
			}
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == ")") {
			throw ParseLispException("syntax error - unexpected ')'");
		} else if (*iter == "'") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "quote"));
			lst.push_back(read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "`") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "`"));
			lst.push_back(read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == ",") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, ","));
			lst.push_back(read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else {
			return _HEAP_ALLOC(env, *iter);
		}
	}

	_VAR parse(Env & env, const string & cmd) {
		vector<string> tokens = tokenize(BufferedCommandReader::eliminateComment(cmd));
		vector<string>::iterator iter = tokens.begin();
		vector<string>::iterator end = tokens.end();
		return read_from_tokens(env, iter, end);
	}

	bool silentsymboleq(_VAR & var, const string & sym) {
		if (var->isSymbol()) {
			return var->r_symbol() == sym;
		}
		return false;
	}

	static _VAR quote(Env & env, Scope & scope, _VAR var) {
		if (var->isList()) {
			vector<_VAR> lst = var->r_list();
			if (lst.empty()) {
				return _NIL(env);
			}
			if (lst[0]->isSymbol() && lst[0]->r_symbol() == ",") {
				throw EvalLispException("unexpected ','");
			}
			vector<_VAR> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				ret.push_back(quote(env, scope, lst[i]));
			}
			return _HEAP_ALLOC(env, ret);
		}
		return var;
	}
	
	static _VAR quasi(Env & env, Scope & scope, _VAR var) {
		if (var->isList()) {
			vector<_VAR> lst = var->r_list();
			if (lst.empty()) {
				return _NIL(env);
			}
			if (lst[0]->isSymbol() && lst[0]->r_symbol() == ",") {
				return eval(env, scope, lst[1]);
			}
			vector<_VAR> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				ret.push_back(quasi(env, scope, lst[i]));
			}
			return _HEAP_ALLOC(env, ret);
		}
		return var;
	}

	_VAR eval(Env & env, const _VAR & var) {
		return eval(env, env.scope(), var);
	}

	_VAR eval(Env & env, Scope & scope, const _VAR & var) {
		if (var->isSymbol()) {
			_VAR v = scope.rsearch(var->r_symbol());
			if (v.nil() == true || v->isFunction()) {
				throw LispException("unbound variable - '" + var->r_symbol() + "'");
			}
			return v;
		} else if (var->isList() == false) {
			return var;
		} else if (var->r_list().empty()) {
			return _NIL(env);
		} else {
			vector<_VAR> & lv = var->r_list();
			_VAR & cmd = lv[0];
			vector<_VAR> args(lv.begin() + 1, lv.end());
			if (silentsymboleq(cmd, "quit")) {
				throw ExitLispException((args.size() > 0 ? *eval(env, scope, args[0])->r_integer() : 0));
			} else if (silentsymboleq(cmd, "lambda")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				args[0]->typeCheck(Var::LIST);
				return _HEAP_ALLOC(env, Func(args[0], args[1]));
			} else if (silentsymboleq(cmd, "defun")) {
				_CHECK_ARGS_MIN_COUNT(args, 3);
				args[1]->typeCheck(Var::LIST);
				scope.rget(args[0]->r_symbol()) = _HEAP_ALLOC(env, Func(args[1], args[2]));
				return scope.rget(args[0]->r_symbol());
			} else if (silentsymboleq(cmd, "setf")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR var = eval(env, scope, args[0]);
				_VAR other = eval(env, scope, args[1]);
				if (var->isList() && other->isList()) {
					for (size_t i = 0; i < var->r_list().size() && i < other->r_list().size(); i++) {
						(*var->r_list()[i]) = (*other->r_list()[i]);
					}
				} else {
					*var = *other;
				}
				return var;
			} else if (silentsymboleq(cmd, "setq")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR v = eval(env, scope, args[1]);
				scope.rget(args[0]->r_symbol()) = v;
				return v;
			} else if (silentsymboleq(cmd, "quote")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				return quote(env, scope, args[0]);
			} else if (silentsymboleq(cmd, "`")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				return quasi(env, scope, args[0]);
			} else if (silentsymboleq(cmd, "function")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				return function(env, scope, args[0]);
			} else if (silentsymboleq(cmd, "funcall")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				_VAR funcsym = eval(env, scope, args[0]);
				_VAR func = function(env, scope, funcsym);
				vector<_VAR> fargs(args.begin() + 1, args.end());
				return func->proc(env, scope, fargs);
			} else if (silentsymboleq(cmd, "let")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				_VAR ret = _NIL(env);
				vector<_VAR> & lets = args[0]->r_list();
				Scope local_scope;
				local_scope.parent() = Ref<Scope>(&scope);
				for (vector<_VAR>::iterator iter = lets.begin(); iter != lets.end(); iter++) {
					vector<_VAR> decl = (*iter)->r_list();
					string sym = decl[0]->r_symbol();
					local_scope.put(sym, eval(env, scope, decl[1]));
				}
				for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
					ret = eval(env, local_scope, *iter);
				}
				return ret;
			} else if (silentsymboleq(cmd, "if")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR val = eval(env, scope, args[0]);
				if (!val->isNil()) {
					return eval(env, scope, args[1]);
				} else if (args.size() > 2) {
					return eval(env, scope, args[2]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "when")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR test = eval(env, scope, args[0]);
				if (!test->isNil()) {
					return eval(env, scope, args[1]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "unless")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR test = eval(env, scope, args[0]);
				if (test->isNil()) {
					return eval(env, scope, args[1]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "cond")) {
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					vector<_VAR> lst = (*iter)->r_list();
					if (!eval(env, scope, lst[0])->isNil()) {
						return eval(env, scope, lst[1]);
					}
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "progn")) {
				_VAR ret = _NIL(env);
				_FORI(args, i, 0) {
					ret = eval(env, scope, args[i]);
				}
				return ret;
			} else if (silentsymboleq(cmd, "while")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR pre_test = args[0];
				while (!eval(env, scope, pre_test)->isNil()) {
					eval(env, scope, args[1]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "dolist")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				vector<_VAR> decl = args[0]->r_list();
				string param = decl[0]->r_symbol();
				vector<_VAR> lst = eval(env, scope, decl[1])->r_list();
				Scope local_scope;
				local_scope.parent() = Ref<Scope>(&scope);
				for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
					local_scope.put(param, *iter);
					eval(env, local_scope, args[1]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "dotimes")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				vector<_VAR> steps = args[0]->r_list();
				string sym = steps[0]->r_symbol();
				Integer limit = eval(env, scope, steps[1])->r_integer();
				Scope local_scope;
				local_scope.parent() = Ref<Scope>(&scope);
				local_scope.put(sym, _HEAP_ALLOC(env, Integer(0)));
				for (; local_scope.get(sym)->r_integer() < limit;
					 local_scope.put(sym, _HEAP_ALLOC(env, local_scope.get(sym)->r_integer() + 1))) {
					eval(env, local_scope, args[1]);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "loop")) {
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
				throw LispException("not implemeneted");
			} else if (silentsymboleq(cmd, "list")) {
				vector<_VAR> elts;
				for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
					elts.push_back(eval(env, scope, *iter));
				}
				return _HEAP_ALLOC(env, elts);
			} else if (silentsymboleq(cmd, "cons")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				vector<_VAR> ret;
				_VAR cons = eval(env, scope, args[0]);
				_VAR cell = eval(env, scope, args[1]);
				ret.push_back(cons);
				if (cell->isList()) {
					vector<_VAR> lst = cell->r_list();
					ret.insert(ret.end(), lst.begin(), lst.end());
				} else {
					ret.push_back(cell);
				}
				return _HEAP_ALLOC(env, ret);
			} else if (silentsymboleq(cmd, "car")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				vector<_VAR> & lst = eval(env, scope, args[0])->r_list();
				if (lst.size() > 0) {
					return lst[0];
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "cdr")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				vector<_VAR> & lst = eval(env, scope, args[0])->r_list();
				if (lst.size() > 1) {
					vector<_VAR> rest;
					for (vector<_VAR>::iterator iter = lst.begin() + 1; iter != lst.end(); iter++) {
						rest.push_back(*iter);
					}
					return _HEAP_ALLOC(env, rest);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "nth")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				size_t idx = (size_t)(*(eval(env, scope, args[0])->r_integer()));
				vector<_VAR> & lst = eval(env, scope, args[1])->r_list();
				if (idx < lst.size()) {
					return lst[idx];
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "nthcdr")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				size_t idx = (size_t)(*eval(env, scope, args[0])->r_integer());
				vector<_VAR> & lst = eval(env, scope, args[1])->r_list();
				if (idx < lst.size()) {
					vector<_VAR> rest;
					for (vector<_VAR>::iterator iter = lst.begin() + idx; iter != lst.end(); iter++) {
						rest.push_back(*iter);
					}
					return _HEAP_ALLOC(env, rest);
				}
				return _NIL(env);
			} else if (silentsymboleq(cmd, "subseq")) {
				_CHECK_ARGS_MIN_COUNT(args, 3);
				vector<_VAR> & lst = eval(env, scope, args[0])->r_list();
				Integer start = eval(env, scope, args[1])->r_integer();
				Integer end = eval(env, scope, args[2])->r_integer();
				vector<_VAR> ret;
				for (size_t i = (size_t)*start; i < (size_t)*end && i < lst.size(); i++) {
					ret.push_back(lst[i]);
				}
				return _HEAP_ALLOC(env, ret);
			} else if (silentsymboleq(cmd, "unwind-protect")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				_VAR ret = _NIL(env);
				try {
					ret = eval(env, scope, args[0]);
				} catch (LispException e) {
					eval(env, scope, args[1]);
					throw e;
				}
				eval(env, scope, args[1]);
				return ret;
			} else if (silentsymboleq(cmd, "catch")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				try {
					_VAR ret = _NIL(env);
					_FORI(args, i, 1) {
						ret = eval(env, scope, args[i]);
					}
					return ret;
				} catch (ThrowLispException e) {
					_VAR exp = eval(env, scope, args[0]);
					if (_EQ_NIL_OR_SYMBOL(e.except(), exp)) {
						return e.ret();
					}
					throw e;
				}
			} else if (silentsymboleq(cmd, "throw")) {
				_CHECK_ARGS_MIN_COUNT(args, 2);
				throw ThrowLispException(eval(env, scope, args[0]), eval(env, scope, args[1]));
			} else if (silentsymboleq(cmd, "block")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				try {
					_VAR ret = _NIL(env);
					_FORI(args, i, 1) {
						ret = eval(env, scope, args[i]);
					}
					return ret;
				} catch (ReturnLispException e) {
					if (_EQ_NIL_OR_SYMBOL(e.tag(), args[0])) {
						return e.var();
					}
					throw e;
				}
			} else if (silentsymboleq(cmd, "return-from")) {
				_CHECK_ARGS_MIN_COUNT(args, 1);
				_VAR ret = _OPT_EVAL(env, scope, args, 1, _NIL(env));
				throw ReturnLispException(args[0], ret);
			} else if (silentsymboleq(cmd, "defmacro")) {
				// TODO: implement
				// refer [http://clhs.lisp.se/Body/m_defmac.htm]
				throw LispException("not implemeneted");
			} else if (silentsymboleq(cmd, "macroexpand")) {
				// TODO: implement
				throw LispException("not implemeneted");
			} else {
				_VAR func = function(env, scope, cmd);
				return func->proc(env, scope, cmd, args);
			}
		}
		return _NIL(env);
	}

	_VAR compile(Env & env, const string & cmd) {
		return eval(env, env.scope(), parse(env, cmd));
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
		DECL_NATIVE_BEGIN(env, "symbolp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isSymbol());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "listp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isList());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "booleanp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isBoolean());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "integerp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isInteger());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "floatp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFloat());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "stringp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isString());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "funcp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFunction());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathnamep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFile());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "streamp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFileDescriptor());
		}
		DECL_NATIVE_END();
	}

	void builtin_algorithm(Env & env) {

		// remove

		// TODO: refer - [http://www.lispworks.com/documentation/lw60/CLHS/Body/f_map.htm]
		DECL_NATIVE_BEGIN(env, "map");
		{
			_CHECK_ARGS_MIN_COUNT(args, 3);
			// TODO: check - http://clhs.lisp.se/Body/f_map.htm
			_VAR result_type = eval(env, scope, args[0]); /* TODO: use it */
			_VAR func = function(env, scope, eval(env, scope, args[1]));
			_VAR seq = eval(env, scope, args[2]); /* TODO: use it */

			vector<_VAR> ret;

			vector<vector<_VAR> > lists;
			size_t size = 0;
			for (size_t i = 2; i < args.size(); i++) {
				vector<_VAR> lst = eval(env, scope, args[i])->r_list();
				if (lst.size() > size) {
					size = lst.size();
				}
				lists.push_back(lst);
			}

			for (size_t i = 0; i < size; i++) {
				vector<_VAR> fargs;
				for (vector<vector<_VAR> >::iterator iter = lists.begin(); iter != lists.end(); iter++) {
					vector<_VAR> & lst = (*iter);
					fargs.push_back((i < lst.size() ? lst[i] : _NIL(env)));
				}
				ret.push_back(func->proc(env, scope, fargs));
			}

			return _HEAP_ALLOC(env, ret);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "sort");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			vector<_VAR> lst = eval(env, scope, args[0])->r_list();
			_VAR func = eval(env, scope, args[1]);
			func = function(env, scope, func);

			if (lst.size() <= 1) {
				return _HEAP_ALLOC(env, lst);
			}

			for (size_t loop = 0; loop < lst.size() - 1; loop++) {
				for (size_t i = 0; i < lst.size() - 1; i++) {
					vector<_VAR> fargs;
					fargs.push_back(lst[i]);
					fargs.push_back(lst[i + 1]);
					if (!func->proc(env, scope, _HEAP_ALLOC(env, "#sort"), fargs)->isNil()) {
						iter_swap(lst.begin() + i, lst.begin() + (i + 1));
					}
				}
			}
			return _HEAP_ALLOC(env, lst);
		}
		DECL_NATIVE_END();
	}

	void builtin_list(Env & env) {
		DECL_NATIVE_BEGIN(env, "length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			vector<_VAR> lst = eval(env, scope, args[0])->r_list();
			return _HEAP_ALLOC(env, Integer((long long)lst.size()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			vector<_VAR> ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				vector<_VAR> lst = eval(env, scope, *iter)->r_list();
				ret.insert(ret.end(), lst.begin(), lst.end());
			}
			return _HEAP_ALLOC(env, ret);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "remove");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR val = eval(env, scope, args[0]);
			vector<_VAR> lst = eval(env, scope, args[1])->r_list();
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				if (val->toString() == (*iter)->toString()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "remove-if");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = eval(env, scope, args[0]);
			func = function(env, scope, func);
			vector<_VAR> lst = eval(env, scope, args[1])->r_list();
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				vector<_VAR> fargs;
				fargs.push_back(*iter);
				if (!func->proc(env, scope, fargs)->isNil()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}
		DECL_NATIVE_END();

		// TODO: implement
		// [https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html]
		// remove-if-not
		// [http://www.jtra.cz/stuff/lisp/sclr/push.html]
		// push
		// pop
	}

	void builtin_logic(Env & env) {
		DECL_NATIVE_BEGIN(env, "not");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isNil());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "or");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR var = _NIL(env);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				var = eval(env, scope, *iter);
				if (!var->isNil()) {
					break;
				}
			}
			return var;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "and");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR var = _HEAP_ALLOC(env, "t");
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				var = eval(env, scope, *iter);
				if (var->isNil()) {
					break;
				}
			}
			return var;
		}
		DECL_NATIVE_END();
	}

	void builtin_string(Env & env) {
		DECL_NATIVE_BEGIN(env, "string=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if (val != eval(env, scope, *iter)->toString()) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-prefix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toString();
			string dst = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, Text::startsWith(str, dst));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-suffix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toString();
			string dst = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, Text::endsWith(str, dst));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer len((long long)eval(env, scope, args[0])->toString().length());
			return _HEAP_ALLOC(env, len);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			string ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				ret.append(eval(env, scope, *iter)->toString());
			}
			return _HEAP_ALLOC(env, wrap_text(ret));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "format");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			vector<_VAR> fargs(args.begin() + 2, args.end());
			string str = format(env, scope, args[1]->toString(), fargs);
			if (!test->isNil()) {
				fputs(str.c_str(), stdout);
				fputs("\n", stdout);
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, wrap_text(str));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "enough-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string org = eval(env, scope, args[0])->toString();
			string prefix = eval(env, scope, args[1])->toString();
			if (Text::startsWith(org, prefix)) {
				return _HEAP_ALLOC(env, wrap_text(org.substr(prefix.length())));
			}
			return _HEAP_ALLOC(env, wrap_text(org));
		}
		DECL_NATIVE_END();
	}
	void builtin_artithmetic(Env & env) {
		DECL_NATIVE_BEGIN(env, "=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			Integer val = v->r_integer();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if (!eq(env, v, eval(env, scope, *iter))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "+");
		{
			_VAR v = _HEAP_ALLOC(env, 0);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = plus(env, v, eval(env, scope, *iter));
			}
			return v;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "-");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);				
			if (args.size() == 1) {
				return minus(env, _HEAP_ALLOC(env, 0), eval(env, scope, args[0]));
			}
			_VAR v = eval(env, scope, args[0]);
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				v = minus(env, v, eval(env, scope, *iter));
			}
			return v;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "*");
		{
			_VAR v = _HEAP_ALLOC(env, 1);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = multiply(env, v, eval(env, scope, *iter));
			}
			return v;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "/");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				v = divide(env, v, eval(env, scope, *iter));
			}
			return v;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "%");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer sum = eval(env, scope, args[0])->r_integer();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				sum %= eval(env, scope, *iter)->r_integer();
			}
			return _HEAP_ALLOC(env, sum);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, ">");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			return _HEAP_ALLOC(env, gt(env, eval(env, scope, args[0]), eval(env, scope, args[1])));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			return _HEAP_ALLOC(env, lt(env, eval(env, scope, args[0]), eval(env, scope, args[1])));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, ">=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			return _HEAP_ALLOC(env, gteq(env, eval(env, scope, args[0]), eval(env, scope, args[1])));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			return _HEAP_ALLOC(env, lteq(env, eval(env, scope, args[0]), eval(env, scope, args[1])));
		}
		DECL_NATIVE_END();
	}
	void builtin_io(Env & env) {

		env.scope().put("*standard-output*", _HEAP_ALLOC(env, FileDescriptor(stdout)));
		env.scope().put("*standard-input*", _HEAP_ALLOC(env, FileDescriptor(stdin)));

		DECL_NATIVE_BEGIN(env, "read");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret = _NIL(env);
			FileDescriptor fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd.eof()) {
				return _HEAP_ALLOC(env, true);
			}
			BufferedCommandReader reader;
			while (!fd.eof() && reader.read(fd.readline() + "\n") < 1) {}
                
			vector<string> commands = reader.getCommands();
			for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
				ret = compile(env, *iter);
				env.gc();
			}
			return ret;

        }
		DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "read-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd.eof()) {
				return _HEAP_ALLOC(env, true);
			}
			string line = fd.readline();
			return _HEAP_ALLOC(env, wrap_text(line));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "print");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor fd = scope.rsearch("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd.write(msg);
			fd.write("\n");
			return _HEAP_ALLOC(env, wrap_text(msg));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "write-string");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor fd = scope.rsearch("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd.write(msg);
			return _HEAP_ALLOC(env, wrap_text(msg));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "write-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor fd = scope.rsearch("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd.write(msg);
			fd.write("\n");
			return _HEAP_ALLOC(env, wrap_text(msg));
		}
		DECL_NATIVE_END();
	}
	void builtin_pathname(Env & env) {
		DECL_NATIVE_BEGIN(env, "pathname");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR path = pathname(env, eval(env, scope, args[0]));
			return path;
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathname-name");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getFileNameWithoutExtension()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathname-type");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getExtension()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getPath()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "directory-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getDirectory()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getFileName()));
		}
		DECL_NATIVE_END();
		
		// https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html
		
		DECL_NATIVE_BEGIN(env, "directory-file-name");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			string path = file.getPath();
			if (path.empty()) {
				return _NIL(env);
			}
			if (File::getSeparators().find(*path.rbegin()) != string::npos) {
				return _HEAP_ALLOC(env, wrap_text(path.substr(0, path.size() - 1)));
			}
			return _HEAP_ALLOC(env, wrap_text(path));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-name-directory");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			string path = file.getPath();
			size_t f = path.find_last_of(File::getSeparators());
			if (f == string::npos) {
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, wrap_text(path.substr(0, f+1)));
		}
		DECL_NATIVE_END();
	}
	void builtin_file(Env & env) {
		DECL_NATIVE_BEGIN(env, "dir");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR path = ((args.size() > 0) ? pathname(env, eval(env, scope, args[0])) : _HEAP_ALLOC(env, "#p\".\""));
			vector<File> files = File::list(path->r_file().getPath());
			vector<_VAR> lst;
			for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
				lst.push_back(_HEAP_ALLOC(env, *iter));
			}
			return _HEAP_ALLOC(env, lst);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "probe-file");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return file.exists() ? _HEAP_ALLOC(env, file) : _NIL(env);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "dirp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, file.isDirectory());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "filep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, file.isFile());
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)file.getSize()));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-attribute-creation");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.creationTime()).sec));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-attribute-lastmodified");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.lastModifiedTime()).sec));
		}
		DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "open");
		{
			map<string, _VAR> keywords = Arguments::extractKeywords(args);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			const char * flags = "rb+";
			if (!file.exists()) {
				// does not exists
				if (_CONTAINS(keywords, ":if-does-not-exist")) {
					if (keywords[":if-does-not-exist"]->isNil()) {
						return _NIL(env);
					} else if (keywords[":if-does-not-exist"]->r_symbol() == ":create") {
						flags = "wb+";
					}
				}
			} else {
				// exists
				if (_CONTAINS(keywords, ":if-exists")) {
					if (keywords[":if-exists"]->isNil()) {
						return _NIL(env);
					} else if (keywords[":if-exists"]->r_symbol() == ":append") {
						flags = "ab+";
					} else if (keywords[":if-exists"]->r_symbol() == ":overwrite") {
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
			return _HEAP_ALLOC(env, FileDescriptor(fp));
#elif defined(USE_MS_WIN)
			FILE * fp = NULL;
			if (fopen_s(&fp, file.getPath().c_str(), flags) != 0) {
				throw LispException("Cannot open file");
			}
			return _HEAP_ALLOC(env, FileDescriptor(fp));
#endif		
		}
		DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "file-position");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (args.size() > 1) {
				fd.position((size_t)*eval(env, scope, args[1])->r_integer());
			}
			return _HEAP_ALLOC(env, Integer((long long)fd.position()));
				
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "close");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			eval(env, scope, args[0])->r_fileDescriptor().close();
			return _NIL(env);
		}
		DECL_NATIVE_END();
	}
	void builtin_socket(Env & env) {
		// TODO: implement
	}
	void builtin_system(Env & env) {
		DECL_NATIVE_BEGIN(env, "system");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer ret(system(eval(env, scope, args[0])->toString().c_str()));
			return _HEAP_ALLOC(env, ret);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "load");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			FileStream stream(file, "rb");
			string dump = stream.readFullAsString();
			stream.close();
			vector<string> lines = Text::split(dump, "\n");
			BufferedCommandReader reader;
			for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
				if (reader.read(*iter + "\n") > 0) {
					vector<string> commands = reader.getCommands();
					for (vector<string>::iterator cmd = commands.begin(); cmd != commands.end(); cmd++) {
						compile(env, *cmd);
						env.gc();
					}
				}
			}
			return _HEAP_ALLOC(env, true);
		}
		DECL_NATIVE_END();
	}
	
	void builtin_date(Env & env) {
		env.scope().put("internal-time-units-per-second", _HEAP_ALLOC(env, 1000));
		DECL_NATIVE_BEGIN(env, "now");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			char buffer[512];
			memset(buffer, 0, sizeof(buffer));
			Date date = Date::now();
			snprintf(buffer, sizeof(buffer), "%04d-%02d-%02d %02d:%02d:%02d.%d",
					 date.getYear(), date.getMonth() + 1, date.getDay(),
					 date.getHour(), date.getMinute(), date.getSecond(),
					 date.getMillisecond());
			return _HEAP_ALLOC(env, wrap_text(buffer));
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "get-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			return _HEAP_ALLOC(env, (long long)osl_get_time_network().sec);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "decode-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			// TODO: apply specific zone
			osl_time_t time = {0,};
			time.sec = (unsigned long)*eval(env, scope, args[0])->r_integer();
			time = osl_network_time_to_system_time(time);
			Date date(time);
			vector<_VAR> ret;
			ret.push_back(_HEAP_ALLOC(env, date.getSecond()));
			ret.push_back(_HEAP_ALLOC(env, date.getMinute()));
			ret.push_back(_HEAP_ALLOC(env, date.getHour()));
			ret.push_back(_HEAP_ALLOC(env, date.getDay()));
			ret.push_back(_HEAP_ALLOC(env, date.getMonth() + 1));
			ret.push_back(_HEAP_ALLOC(env, date.getYear()));
			// day of week
			// daylight-p
			// zone
			return _HEAP_ALLOC(env, ret);
		}
		DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "encode-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 6); // seconds, minutes, hours, dates, month and year (gmt offset)
			Date date = Date::now();
			date.setSecond((int)*args[0]->r_integer());
			date.setMinute((int)*args[1]->r_integer());
			date.setHour((int)*args[2]->r_integer());
			date.setDay((int)*args[3]->r_integer());
			date.setMonth((int)((*args[4]->r_integer()) - 1));
			date.setYear((int)*args[5]->r_integer());
			if (args.size() > 6) {
				date.setGmtOffset((int)*args[6]->r_integer());
			}
			return _HEAP_ALLOC(env, (long long)osl_system_time_to_network_time(date.getTime()).sec);
		}
		DECL_NATIVE_END();
	}

	/**
	 * @brief buffered command reader
	 */

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
	string BufferedCommandReader::eliminateComment(const string & text) {
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
		size_t i = text.find_first_not_of(" \t\r\n'`");
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
		buffer = eliminateComment(buffer);
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
			return var->r_string();
		}
		return var->toString();
	}

	void repl(Env & env) {
		BufferedCommandReader reader;
		char line[4096] = {0,};
		fputs("> ", stdout);
		while (fgets(line, sizeof(line), stdin)) {
			if (reader.read(line) > 0) {
				vector<string> & commands = reader.getCommands();
				for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
					fputs(printVar(compile(env, *iter)).c_str(), stdout);
					fputs("\n", stdout);
					env.gc();
				}
				reader.clearCommands();
				return;
			}
		}
	}
}
