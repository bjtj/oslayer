#include <cmath>
#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "Iterator.hpp"
#include "FileStream.hpp"
#include "Socket.hpp"
#include "AutoLock.hpp"
#include "Process.hpp"
#include "DatabaseDriver.hpp"
#include "DatabaseConnection.hpp"

#define _CONTAINS(M,E) (M.find(E) != M.end())
#define _HEAP_ALLOC(E,V) E.alloc(new Var(V))
#define _VAR GCRef<Var>
#define _NIL(e) _HEAP_ALLOC(e,"nil")
#define _CHECK_ARGS_MIN_COUNT(L,C) validateArgumentCountMin(L,C)
#define _CHECK_ARGS_EXACT_COUNT(L,C) validateArgumentCountExact(L,C)
#define _CHECK_ARGS_MAX_COUNT(L,C) validateArgumentCountMax(L,C)
#define _CHECK_ARGS_EVEN_COUNT(L) validateArgumentCountEven(L)
#define _EQ_NIL_OR_SYMBOL(A,B) (((A)->isNil() && (B)->isNil()) ||		\
								(((A)->isNil() == false && (B)->isNil() == false) && \
								 ((A)->r_symbol() == (B)->r_symbol())))
#define _OPT_EVAL(E,S,L,N,D) (N < L.size() ? eval(E, S, L[N]) : D)
#define _FORI(L,I,F) for (size_t I = (F); I < (L).size(); I++)
#define _FORI_STEP(L,I,F,S) for (size_t I = (F); I < (L).size(); I += (S))

#define _CHAR(V) (V->r_character().raw())
#define _INT(V) (V->r_integer().raw())
#define _FLOAT(V) (V->r_float().raw())

#define DECL_NATIVE_BEGIN(ENV,NAME)								\
	do {														\
	Env & _E = ENV;												\
	string _N = NAME;											\
	class _cls : public Procedure {								\
	private:														\
	public:															\
	_cls(const string & name) : Procedure(name) {}					\
	virtual ~_cls() {}												\
	virtual _VAR proc(Env & env, AutoRef<Scope> scope, _VAR name, vector<_VAR> & args) {
#define DECL_NATIVE_END()											\
	}																\
	};																\
	_E.scope()->put_func(_N, _E.alloc(new Var(AutoRef<Procedure>(new _cls(_N))))); \
	} while (0);

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	static string printVar(_VAR var);

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
		: _exc(except), _ret(ret) {
	}
	ThrowLispException::~ThrowLispException() throw() {
	}
	_VAR ThrowLispException::except() {
		return _exc;
	}
	_VAR ThrowLispException::ret() {
		return _ret;
	}

	/**
	 * unbound 
	 */
	
	UnboundLispException::UnboundLispException(const string & name) : _name(name) {
	}
	UnboundLispException::~UnboundLispException() throw() {
	}
	string UnboundLispException::toString() const {
		return "unbound variable - '" + _name + "'";
	}
	string & UnboundLispException::name() {
		return _name;
	}

	/**
	 * @brief registry
	 */

	Registry::Registry() {
	}

	Registry::~Registry() {
	}

	bool Registry::contains(const string & k) {
		return (find(k) != end() ? true : false);
	}
	
	/**
	 * @brief scope
	 */

	Scope::Scope() {
	}
	
	Scope::~Scope() {
	}
	
	AutoRef<Scope> & Scope::parent() {
		return _parent;
	}
	void Scope::clear() {
		_registries.clear();
	}
	
	map<string, Registry> & Scope::registries() {
		return _registries;
	}
	
	Registry & Scope::registry(const string id) {
		return _registries[id];
	}
	
	_VAR Scope::rsearch_sym(const string & name) {
		return rsearch("symbol", name);
	}
	
	_VAR Scope::rget_sym(const string & name) {
		return rget("symbol", name);
	}
	
	_VAR Scope::rput_sym(const string & name, const _VAR & var) {
		return rput("symbol", name, var);
	}
	
	_VAR Scope::rsearch_func(const string & name) {
		return rsearch("function", name);
	}

	_VAR Scope::rget_func(const string & name) {
		return rget("function", name);
	}

	_VAR Scope::rput_func(const string & name, const _VAR & var) {
		return rput("function", name, var);
	}

	_VAR Scope::rsearch(const string id, const string & name) {
		if (registry(id).contains(name)) {
			return registry(id)[name];
		}
		if (_parent.nil() == false) {
			return _parent->rsearch(id, name);
		}
		return _VAR();
	}

	_VAR Scope::rget(const string id, const string & name) {
		if (registry(id).contains(name)) {
			return registry(id)[name];
		}
		if (_parent.nil() == false) {
			return _parent->rget(id, name);
		}
		throw UnboundLispException(name);
	}
	_VAR Scope::rput(const string id, const string & name, const _VAR & var) {
		if (registry(id).contains(name)) {
			registry(id)[name] = var;
			return var;
		}
		if (_parent.nil() == false) {
			return _parent->rput(id, name, var);
		}
		registry(id)[name] = var;
		return var;
	}

	_VAR Scope::get_sym(const string & name) {
		return get("symbol", name);
	}
	
	void Scope::put_sym(const string & name, const _VAR & var) {
		put("symbol", name, var);
	}

	_VAR Scope::get_func(const string & name) {
		return get("function", name);
	}
	
	void Scope::put_func(const string & name, const _VAR & var) {
		put("function", name, var);
	}
	
	_VAR Scope::get(const string id, const string & name) {
		if (registry(id).contains(name) == false) {
			throw UnboundLispException(name);
		}
		return registry(id)[name];
	}
	
	void Scope::put(const string id, const string & name, const _VAR & var) {
		registry(id)[name] = var;
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
		
		for (map<string, Registry>::const_iterator ri = _registries.begin(); ri != _registries.end(); ri++) {
			ret.append("$$");
			ret.append(ri->first);
			ret.append(": \n");
			ret.append(indent);
			ret.append("[");
			for (map<string, _VAR>::const_iterator it = ri->second.begin(); it != ri->second.end(); it++) {
				if (it != ri->second.begin()) {
					ret.append("\n");
				}
				ret.append(indent);
				ret.append("'");
				ret.append(it->first);
				ret.append("': '");
				ret.append(_to_string(it->second));
				ret.append("'");
			}
			ret.append(indent);
			ret.append("]");
		}
		return ret;
	}

	/**
	 * procedure
	 */

	Procedure::Procedure(const string & name) : _name(name) {
	}
	Procedure::~Procedure() {
	}
	string & Procedure::name() {
		return _name;
	}
	_VAR & Procedure::doc() {
		return _doc;
	}

	/**
	 * 
	 */
	Character::Character() : _ch(0) {
	}
	Character::Character(int ch) : _ch(ch) {
	}
	Character::Character(const string & name) : _ch(0), _name(name) {
		if (_name.size() == 1) {
			_ch = _name[0];
		}
		if (_name == "Backspace") {
			_ch = (char)8;
		}
		if (_name == "Tab") {
			_ch = '\t';
		}
		if (_name == "Newline") {
			_ch = '\n';
		}
		if (_name == "Linefeed") {
			_ch = '\n';
		}
		if (_name == "Space") {
			_ch = ' ';
		}
	}
	Character::~Character() {
	}
	int & Character::raw() {
		return _ch;
	}
	int Character::raw() const {
		return _ch;
	}
	string & Character::name() {
		return _name;
	}
	size_t Character::width() const {
		return sizeof(_ch);
	}
	bool Character::alpha_char_p() const {
		return Text::isAlpha((char)_ch);
	}
	bool Character::alpha_numeric_p() const {
		return Text::isAlphaNumeric((char)_ch);
	}
	bool Character::digit_char_p() const {
		return Text::isDigit((char)_ch);
	}
	bool Character::graphic_char_p() const {
		// todo
		return false;
	}
	bool Character::standard_char_p() const {
		// todo
		return false;
	}
	Character Character::upcase() const {
		return Character(Text::upcase((char)_ch));
	}
	Character Character::downcase() const {
		return Character(Text::downcase((char)_ch));
	}
	bool Character::upper_case_p() const {
		return Text::isUppercase((char)_ch);
	}
	bool Character::lower_case_p() const {
		return Text::isLowercse((char)_ch);
	}
	bool Character::both_case_p() const {
		return (Text::isUppercase((char)_ch) || Text::isLowercse((char)_ch));
	}
	int Character::char_code() const {
		return _ch;
	}
	int Character::char_int() const {
		return _ch;
	}
	int Character::char_code_limit() const {
		// todo
		return 0;
	}
	string Character::charname() const {
		return _name;
	}
	bool Character::equal(const Character & ch) const {
		return upcase() == ch.upcase();
	}
	bool Character::lessp(const Character & ch) const {
		return upcase() < ch.upcase();
	}
	bool Character::greaterp(const Character & ch) const {
		return upcase() > ch.upcase();
	}
	bool Character::operator== (const Character & ch) const {
		return _ch == ch._ch;
	}
	bool Character::operator/= (const Character & ch) const {
		return _ch != ch._ch;
	}
	bool Character::operator< (const Character & ch) const {
		return _ch < ch._ch;
	}
	bool Character::operator> (const Character & ch) const {
		return _ch > ch._ch;
	}
	bool Character::operator<= (const Character & ch) const {
		return _ch <= ch._ch;
	}
	bool Character::operator>= (const Character & ch) const {
		return _ch >= ch._ch;
	}


	/**
	 * 
	 */
	FileDescriptor::FileDescriptor()
		: AutoCloseable<Closeable>(this), _fd(NULL) {
	}
	FileDescriptor::FileDescriptor(bool autoclose)
		: AutoCloseable<Closeable>(this, autoclose), _fd(NULL) {
	}
	FileDescriptor::FileDescriptor(FILE * _fd)
		: AutoCloseable<Closeable>(this), _fd(_fd) {
	}
	FileDescriptor::FileDescriptor(FILE * _fd, bool autoclose)
		: AutoCloseable<Closeable>(this, autoclose), _fd(_fd) {
	}
	FileDescriptor::~FileDescriptor() {
	}
	FILE * FileDescriptor::fd() {
		return _fd;
	}
	void FileDescriptor::testFd() {
		if (!_fd) {
			throw LispException("file descriptor - nil");
		}
	}
	bool FileDescriptor::eof() {
		testFd();
		return feof(_fd) ? true : false;
	}
	int FileDescriptor::read() {
		return fgetc(_fd);
	}
	string FileDescriptor::readline() {
		testFd();
		char buffer[1024] = {0,};
		if (fgets(buffer, sizeof(buffer), _fd)) {
			if (buffer[strlen(buffer) - 1] == '\n') {
				buffer[strlen(buffer) - 1] = '\0';
			}
		}
		return string(buffer);
	}
	void FileDescriptor::write(const string & data) {
		testFd();
		fputs(data.c_str(), _fd);
	}
	size_t FileDescriptor::position() {
		long pos = ftell(_fd);
		if (pos < 0) {
			throw LispException("ftell() error");
		}
		return (size_t)pos;
	}
	void FileDescriptor::position(size_t seek) {
		fseek(_fd, seek, SEEK_SET);
	}
	void FileDescriptor::close() {
		if (_fd) {
			fclose(_fd);
			_fd = NULL;
		}
	}
	
	/**
	 * @brief Env
	 */

	bool Env::_debug = false;

	Env::Env() : _scope(new Scope) {
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

	AutoRef<Scope> & Env::scope() {
		return _scope;
	}
	SharedHeap<Var> & Env::heap() {
		return _heap;
	}
	_VAR Env::alloc(Var * var) {
		AutoLock lock(Ref<Semaphore>(&heap().sem()));
		return heap().alloc(var);
	}
	void Env::gc() {
		AutoLock lock(Ref<Semaphore>(&_heap.sem()));
		size_t size = _heap.size();
		unsigned long elapsed = _heap.gc();
		if (_debug) {
			printf(" # GC / %d, dealloc: %d (%ld ms.) #\n", (int)_heap.size(), (int)(size - _heap.size()), elapsed);
		}
	}
	void Env::clear() {
		_scope->clear();
		gc();
		AutoLock lock(Ref<Semaphore>(&_heap.sem()));
		_heap.clear();
	}
	string & Env::last_command() {
		return _last_command;
	}

	/**
	 * @brief Func
	 */

	Func::Func()
		: _macro(false), _closure_scope(new Scope) {
	}
	Func::Func(const _VAR & params, const _VAR & form)
		: _macro(false), _closure_scope(new Scope), _params(params), _form(form) {
	}
	Func::Func(bool macro, const _VAR & params, const _VAR & form)
		: _macro(macro), _closure_scope(new Scope), _params(params), _form(form) {
	}
	Func::Func(const _VAR & doc, const _VAR & params, const _VAR & form)
		: _macro(false), _closure_scope(new Scope), _doc(doc), _params(params), _form(form) {
	}
	Func::Func(bool macro, const _VAR & doc, const _VAR & params, const _VAR & form)
		: _macro(macro), _closure_scope(new Scope), _doc(doc), _params(params), _form(form) {
	}
	Func::~Func() {
	}
	bool Func::empty() const {
		return _params.nil() || _form.nil() || _params->isNil() || _form->isNil();
	}
	bool & Func::macro() {
		return _macro;
	}
	bool Func::macro() const {
		return _macro;
	}
	AutoRef<Scope> & Func::closure_scope() {
		return _closure_scope;
	}
	_VAR & Func::doc() {
		return _doc;
	}
	_VAR & Func::params() {
		return _params;
	}
	_VAR & Func::form() {
		return _form;
	}
	string Func::toString() const {
		if (_macro) {
			return "#<MACRO>";
		}
		return "#<FUNCTION>";
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
	Var::Var(const Character & ch) : _type(CHARACTER), _ch(ch) {
		_trace("init - character");
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
	Var::Var(AutoRef<FileDescriptor> fd) : _type(FILE_DESCRIPTOR), _fd(fd) {
		_trace("init - FileDescriptor");
	}
	Var::Var(AutoRef<LispExtension> ext) : _type(EXTENSION), _ext(ext) {
		_trace("init - Extension");
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
		if (token[0] == ':') {
			_type = KEYWORD;
			_keyword = token;
		} else if (token == "nil") {
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
		case KEYWORD:
			return "KEYWORD";
		case LIST:
			return "LIST";
		case BOOLEAN:
			return "BOOLEAN";
		case CHARACTER:
			return "CHARACTER";
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
		case EXTENSION:
			return "EXTENSION";
		default:
			break;
		}
		throw LispException("[getTypeString()] unknown variable type / " + Text::toString(_type));
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
	bool Var::isKeyword() const {return _type == KEYWORD;}
	bool Var::isBoolean() const {return _type == BOOLEAN;}
	bool Var::isInteger() const {return _type == INTEGER;}
	bool Var::isFloat() const {return _type == FLOAT;}
	bool Var::isString() const {return _type == STRING;}
	bool Var::isFunction() const {return _type == FUNC;}
	bool Var::isFile() const {return _type == FILE;}
	bool Var::isFileDescriptor() const {return _type == FILE_DESCRIPTOR;}
	bool Var::isExtension() const {return _type == EXTENSION;}
	const string & Var::r_symbol() const {typeCheck(SYMBOL); return _symbol;}
	const string & Var::r_keyword() const {typeCheck(KEYWORD); return _keyword;}
	const Character & Var::r_character() const {typeCheck(CHARACTER); return _ch;};
	const string & Var::r_string() const {typeCheck(STRING); return _str;}
	const vector<_VAR> & Var::r_list() const {typeCheck(LIST); return _lst;}
	const Boolean & Var::r_boolean() const {typeCheck(BOOLEAN); return _bval;}
	const Integer & Var::r_integer() const {typeCheck(INTEGER); return _inum;}
	const Float & Var::r_float() const {typeCheck(FLOAT); return _fnum;}
	const File & Var::r_file() const {typeCheck(FILE); return _file;}
	const Func & Var::r_func() const {typeCheck(FUNC); return _func;}
	string & Var::r_symbol() {typeCheck(SYMBOL); return _symbol;}
	string & Var::r_keyword() {typeCheck(KEYWORD); return _keyword;}
	Character & Var::r_character() {typeCheck(CHARACTER); return _ch;};
	string & Var::r_string() {typeCheck(STRING); return _str;}
	vector<_VAR> & Var::r_list() {typeCheck(LIST); return _lst;}
	Boolean & Var::r_boolean() {typeCheck(BOOLEAN); return _bval;}
	Integer & Var::r_integer() {typeCheck(INTEGER); return _inum;}
	Float & Var::r_float() {typeCheck(FLOAT); return _fnum;}
	File & Var::r_file() {typeCheck(FILE); return _file;}
	Func & Var::r_func() {typeCheck(FUNC); return _func;}
	AutoRef<Procedure> & Var::r_procedure() {typeCheck(FUNC); return _procedure;}
	AutoRef<FileDescriptor> & Var::r_fileDescriptor() {typeCheck(FILE_DESCRIPTOR); return _fd;}
	AutoRef<LispExtension> & Var::r_ext() {typeCheck(EXTENSION); return _ext;}
	_VAR Var::expand(Env & env, AutoRef<Scope> scope, _VAR name, vector<_VAR> & args) {
		if (!isFunction()) {
			throw LispException("Not Function / name: '" + name->toString() + "' / type : '" + getTypeString() + "'");
		}
		if (!_procedure.nil()) {
			throw LispException("Not Macro");
		}
		if (_func.macro() == false) {
			throw LispException("Not Macro");
		}
		Parameters params = Parameters::parse(env, scope, _func.params()->r_list());
		params.bind(env, scope, _func.closure_scope(), args, false);
		_func.closure_scope()->parent() = scope;
		return eval(env, _func.closure_scope(), _func.form());

	}
	_VAR Var::proc(Env & env, AutoRef<Scope> scope, _VAR name, vector<_VAR> & args) {
		if (!isFunction()) {
			throw LispException("not function / name: '" + name->toString() +
								"' / type : '" + getTypeString() + "'");
		}
		if (!_procedure.nil()) {
			return _procedure->proc(env, scope, name, args);
		}
		Parameters params = Parameters::parse(env, scope, _func.params()->r_list());
		if (_func.macro()) {
			params.bind(env, scope, _func.closure_scope(), args, false);
			AutoRef<Scope> local_scope(new Scope(*_func.closure_scope()));
			local_scope->parent() = scope;
			return eval(env, scope, eval(env, local_scope, _func.form()));
		}
		params.bind(env, scope, _func.closure_scope(), args, true);
		AutoRef<Scope> local_scope(new Scope(*_func.closure_scope()));
		local_scope->parent() = scope;
		return eval(env, local_scope, _func.form());
	}
	_VAR Var::proc(Env & env, AutoRef<Scope> scope, vector<_VAR> & args) {
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
		case KEYWORD:
			return _keyword;
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
				ret += printVar(*iter);
			}
			ret += ")";
			return ret;
		}
		case BOOLEAN:
		{
			return _bval.toString();
		}
		case CHARACTER:
		{
			return "#\\" + string(1, (char)_ch.raw());
		}
		case INTEGER:
		{
			return Text::toString(_inum.raw());
		}
		case FLOAT:
		{
			string x = Text::rtrim(Text::toString(_fnum.raw()), "0");
			return (x[x.size() - 1] == '.' ? x + "0" : x);
		}
		case STRING:
			return unwrap_text(_str);
		case FUNC:
		{
			if (!_procedure.nil()) {
				return "#<COMPILED FUNCTION " + _procedure->name() + ">";
			}
			return _func.toString();
		}
		case FILE:
			return "#p\"" + _file.getPath() + "\"";
		case FILE_DESCRIPTOR:
			return "#<FD>";
		case EXTENSION:
			return "#<EXTENSION:" + _ext->toString() + ">";
		default:
			break;
		}
		throw LispException("[toString()] unknown variable type / " + Text::toString(_type));
	}

	/**
	 * @brief 
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

	static void validateArgumentCountMax(vector<_VAR> & args, size_t expect) {
		if (args.size() > expect) {
			throw LispException("Wrong argument count: " + Text::toString(args.size())
								+ " / expected maximum: " + Text::toString(expect));
		}
	}

	static void validateArgumentCountEven(vector<_VAR> & args) {
		if (args.size() % 2 != 0) {
			throw LispException("Wrong argument count: " + Text::toString(args.size())
								+ " / expected : even");
		}
	}

	/**
	 * @brief parameters
	 */
	Parameters::Parameter::Parameter() {
	}
	Parameters::Parameter::Parameter(const _VAR & name)
		: _name(name) {
	}
	Parameters::Parameter::Parameter(const _VAR & name, const _VAR & initial)
		: _name(name), _initial(initial) {
	}
	Parameters::Parameter::~Parameter() {
	}
	bool Parameters::Parameter::empty() const {
		return _name.nil();
	}
	_VAR & Parameters::Parameter::name() {
		return _name;
	}
	_VAR & Parameters::Parameter::initial() {
		return _initial;
	}
	string Parameters::Parameter::toString() const {
		return "{'" + (_name.nil() ? "nil" : _name->toString()) +
			"' with initial '" + (_initial.nil() ? "nil" : _initial->toString()) + "'}";
	}


	Parameters::Parameters() {
	}
	Parameters::Parameters(const vector<Parameter> & names)
		: _names(names) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals)
		: _names(names), _optionals(optionals) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals, const Parameter & rest)
		: _names(names), _optionals(optionals), _rest(rest) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals, const map<string, Parameter> & keywords)
		: _names(names), _optionals(optionals), _keywords(keywords) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals, const Parameter & rest, const map<string, Parameter> & keywords)
		: _names(names), _optionals(optionals), _rest(rest), _keywords(keywords) {
	}
	Parameters::~Parameters() {
	}

	vector<Parameters::Parameter> & Parameters::names() {
		return _names;
	}
	vector<Parameters::Parameter> & Parameters::optionals() {
		return _optionals;
	}
	Parameters::Parameter & Parameters::rest() {
		return _rest;
	}
	map<string, Parameters::Parameter> & Parameters::keywords() {
		return _keywords;
	}

	static bool s_is_reserved_keyword(const string & token) {
		vector<string> lst = tokenize("&optional &rest &key");
		for (vector<string>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
			if (*iter == token) {
				return true;
			}
		}
		return false;
	}

	static vector<_VAR> s_read_tokens(Iterator<_VAR> & iter) {
		vector<_VAR> ret;
		while (iter.has()) {
			if ((*iter)->isSymbol() && s_is_reserved_keyword((*iter)->r_symbol())) {
				break;
			}
			ret.push_back(*iter++);
		}
		return ret;
	}

	Parameters Parameters::parse(Env & env, AutoRef<Scope> scope, vector<_VAR> & tokens) {
		Parameters params;
		Iterator<_VAR> iter(tokens);
		
		vector<_VAR> names = s_read_tokens(iter);
		for (vector<_VAR>::iterator iter = names.begin(); iter != names.end(); iter++) {
			params.names().push_back(Parameter(*iter));
		}
		if (iter.has() && (*iter)->isSymbol() && (*iter)->r_symbol() == "&optional") {
			vector<_VAR> optionals = s_read_tokens(++iter);
			if (optionals.size() == 0) {
				throw LispException("&optional without any element");
			}
			for (vector<_VAR>::iterator iter = optionals.begin(); iter != optionals.end(); iter++) {
				if ((*iter)->isList() && (*iter)->r_list().size() == 2) {
					params.optionals().push_back(Parameter((*iter)->r_list()[0], (*iter)->r_list()[1]));
				} else {
					params.optionals().push_back(Parameter(*iter));
				}
			}
		}
		if (iter.has() && (*iter)->isSymbol() && (*iter)->r_symbol() == "&rest") {
			vector<_VAR> rest = s_read_tokens(++iter);
			if (rest.size() == 0) {
				throw LispException("&rest without var name");
			}
			if (rest.size() > 1) {
				throw LispException("&rest can get only 1 var name");
			}
			params.rest() = Parameter(rest[0]);
		}
		if (iter.has() && (*iter)->isSymbol() && (*iter)->r_symbol() == "&key") {
			vector<_VAR> keys = s_read_tokens(++iter);
			if (keys.size() == 0) {
				throw LispException("&key without any element");
			}
			for (vector<_VAR>::iterator iter = keys.begin(); iter != keys.end(); iter++) {
				if ((*iter)->isList() && (*iter)->r_list().size() == 2) {
					_VAR v = (*iter)->r_list()[0];
					_VAR i = (*iter)->r_list()[1];
					params.keywords()[v->r_symbol()] = Parameter(v, i);
				} else {
					params.keywords()[(*iter)->r_symbol()] = Parameter(*iter);
				}
			}
		}
		return params;
	}

	void Parameters::bind(Env & env, AutoRef<Scope> global_scope, AutoRef<Scope> lex_scope, vector<_VAR> & tokens) {
		bind(env, global_scope, lex_scope, tokens, true);
	}
	void Parameters::bind(Env & env, AutoRef<Scope> global_scope, AutoRef<Scope> lex_scope, vector<_VAR> & tokens, bool proc_eval) {
#define _PROC_VAR(E,S,T) (proc_eval ? eval(E,S,(T)) : (T))
		Iterator<_VAR> tokens_iter(tokens);
		_CHECK_ARGS_MIN_COUNT(tokens, _names.size());
		for (vector<Parameter>::iterator iter = _names.begin(); iter != _names.end(); iter++) {
			_VAR v = _PROC_VAR(env, global_scope, *tokens_iter++);
			lex_scope->put_sym(iter->name()->r_symbol(), v);
		}

		for (vector<Parameter>::iterator iter = _optionals.begin(); iter != _optionals.end(); iter++) {
			_VAR v = (tokens_iter.has() ? _PROC_VAR(env, global_scope, *tokens_iter++) :
					  (iter->initial().nil() ? _NIL(env) : _PROC_VAR(env, global_scope, iter->initial())));
			lex_scope->put_sym(iter->name()->r_symbol(), v);
		}

		if (_rest.empty() == false) {
			lex_scope->put_sym(_rest.name()->r_symbol(), _NIL(env));
		}

		for (map<string, Parameter>::iterator iter = _keywords.begin(); iter != _keywords.end(); iter++) {
			_VAR v = (iter->second.initial().nil() ?
					  _NIL(env) : _PROC_VAR(env, global_scope, iter->second.initial()));
			lex_scope->put_sym(iter->first, v);
		}
		
		if (tokens_iter.has() == false) {
			return;
		}
		
		vector<_VAR> rest_ret;
		vector<_VAR> rest(tokens_iter.iter(), tokens.end());
		for (vector<_VAR>::iterator iter = rest.begin(); iter != rest.end(); iter++) {
			if ((*iter)->isKeyword()) {
				if (_keywords.find((*iter)->r_keyword().substr(1)) == _keywords.end()) {
					throw LispException("Keyword '" + (*iter)->r_keyword() + "' is not provided");
				}
				if (iter + 1 == rest.end()) {
					throw LispException("Unexpected end of tokens / keyword's value is missing");
				}
				string n = (*iter)->r_keyword().substr(1);
				rest_ret.push_back(*iter);
				_VAR v = _PROC_VAR(env, global_scope, *(++iter));
				rest_ret.push_back(v);
				lex_scope->put_sym(n, v);
			} else {
				if (_rest.empty()) {
					throw LispException("&rest is not provided");
				}
				_VAR v = _PROC_VAR(env, global_scope, *iter);
				rest_ret.push_back(v);
			}
		}

		if (_rest.empty() == false) {
			lex_scope->put_sym(_rest.name()->r_symbol(), _HEAP_ALLOC(env, rest_ret));
		}
#undef _PROC_VAR
	}

	string Parameters::toString() const {
		string ret;
		ret.append("names: [");
		for (vector<Parameter>::const_iterator iter = _names.begin(); iter != _names.end(); iter++) {
			if (iter != _names.begin()) {
				ret.append(", ");
			}
			ret.append(iter->toString());
		}
		ret.append("], optionals: [");
		for (vector<Parameter>::const_iterator iter = _optionals.begin(); iter != _optionals.end(); iter++) {
			if (iter != _optionals.begin()) {
				ret.append(", ");
			}
			ret.append(iter->toString());
		}
		ret.append("], rest: '");
		ret.append(_rest.toString());
		ret.append("', keywods: [");
		for (map<string, Parameter>::const_iterator iter = _keywords.begin(); iter != _keywords.end(); iter++) {
			if (iter != _keywords.begin()) {
				ret.append(", ");
			}
			ret.append(iter->second.toString());
		}
		ret.append("]");
		return ret;
	}

	// ext

	/**
	 * @brief Server
	 */
	class LispServerSocket : public LispExtension {
	private:
		ServerSocket _server;
	public:
		LispServerSocket(int port) : _server(port) {
			_server.setReuseAddr(true);
			_server.bind();
			_server.listen(50);
		}
		virtual ~LispServerSocket() {
			_server.close();
		}
		ServerSocket & server() {
			return _server;
		}
		AutoRef<Socket> accept() {
			return AutoRef<Socket>(_server.accept());
		}
		virtual string toString() const {
			return "LispServerSocket";
		}
	};

	/**
	 * @brief socket
	 */
	class LispSocket : public LispExtension {
	private:
		AutoRef<Socket> _socket;
	public:
		LispSocket(Socket * socket) : _socket(socket) {
		}
		LispSocket(AutoRef<Socket> socket) : _socket(socket) {
		}
		virtual ~LispSocket() {
			_socket->close();
		}
		AutoRef<Socket> & socket() {
			return _socket;
		}
		int recv(char * buf, size_t size) {
			return _socket->recv(buf, size);
		}
		int send(_VAR v) {
			switch (v->getType()) {
			case Var::STRING:
			{
				string str = v->toString();
				return _socket->send(str.c_str(), str.size());
			}
			case Var::INTEGER:
			{
				char d = (char)*v->r_integer();
				return _socket->send(&d, 1);
			}
			default:
				throw LispException("Not supported variable type - " + v->getTypeString());
			}
		}
		virtual string toString() const {
			return "LispSocket";
		}
	};

	/**
	 *
	 */
	class LispDatabaseConnection : public LispExtension {
	private:
		AutoRef<DatabaseConnection> _conn;
	public:
		LispDatabaseConnection(AutoRef<DatabaseConnection> conn)
			: _conn(conn) {
		}
		virtual ~LispDatabaseConnection() {
		}

		void connect(const string & host, int port, const string & username, const string & password, const string & dbname) {
			_conn->connect(host, port, username, password, dbname);
		}

		void disconnect() {
			if (_conn->isConnected() == false) {
				throw LispException("Not connected yet");
			}
			_conn->disconnect();
		}

		AutoRef<ResultSet> query(const string & sql) {
			if (_conn->isConnected() == false) {
				throw LispException("Not connected yet");
			}
			return _conn->query(sql);
		}

		int queryUpdate(const string & sql) {
			if (_conn->isConnected() == false) {
				throw LispException("Not connected yet");
			}
			return (int)_conn->queryUpdate(sql);
		}

		virtual string toString() const {
			return "LispDatabaseConnection";
		}			
	};

	/**
	 *
	 */
	class LispResultSet : public LispExtension {
	private:
		AutoRef<ResultSet> _resultSet;
	public:
		LispResultSet(AutoRef<ResultSet> resultSet)
			: _resultSet(resultSet) {
		}
		virtual ~LispResultSet() {
		}
		bool next() {
			return _resultSet->next();
		}
		int fieldCount() {
			return _resultSet->fieldCount();
		}
		string getString(int i) {
			return _resultSet->getString(i);
		}
		virtual string toString() const {
			return "LispResultSet";
		}
	};

	// built-in
	static void builtin_essential(Env & env);
	static void builtin_type(Env & env);
	static void builtin_algorithm(Env & env);
	static void builtin_list(Env & env);
	static void builtin_logic(Env & env);
	static void builtin_character(Env & env);
	static void builtin_string(Env & env);
	static void builtin_arithmetic(Env & env);
	static void builtin_mathematic(Env & env);
	static void builtin_io(Env & env);
	static void builtin_pathname(Env & env);
	static void builtin_file(Env & env);
	static void builtin_socket(Env & env);
	static void builtin_concurrent(Env & env);
	static void builtin_system(Env & env);
	static void builtin_date(Env & env);
	static void builtin_macro(Env & env);
	static void builtin_db(Env & env);
	
	static string format(Env & env, AutoRef<Scope> scope, const string & fmt, vector<_VAR> & args) {
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
					num = Text::format("%.2lf", _FLOAT(var));
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

	bool numberp(_VAR v) {
		return (v->isInteger() || v->isFloat());
	}

	bool zerop(_VAR v) {
		if (v->isInteger()) {
			return v->r_integer().zerop();
		} else if (v->isFloat()) {
			return v->r_float().zerop();
		}
		throw LispException("Not number - '" + v->toString() + "'");
	}

	_VAR toFloat(Env & env, _VAR v) {
		if (v->isInteger()) {
			return _HEAP_ALLOC(env, (double)(_INT(v)));
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
		if (zerop(v2)) {
			throw DivisionByZeroLispException("dvision by zero");
		}
		if (v1->isFloat() || v2->isFloat()) {
			v1 = toFloat(env, v1);
			v2 = toFloat(env, v2);
			
			return _HEAP_ALLOC(env, v1->r_float() / v2->r_float());
		}
		return _HEAP_ALLOC(env, v1->r_integer() / v2->r_integer());
	}

	static _VAR _cos(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, cos((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, cos(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _sin(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, sin((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, sin(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _tan(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, tan((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, tan(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _acos(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, acos((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, acos(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _asin(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, asin((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, asin(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _atan(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, atan((double)_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, atan(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	static _VAR _abs(Env & env, _VAR v) {
		if (numberp(v) == false) {
			throw LispException("numberp failed");
		}
		switch (v->getType()) {
		case Var::INTEGER:
			return _HEAP_ALLOC(env, (int)abs(_INT(v)));
		case Var::FLOAT:
			return _HEAP_ALLOC(env, abs(_FLOAT(v)));
		default:
			break;
		}
		throw LispException("unknown exception");
	}

	/**
	 * get function
	 */
	static _VAR function(Env & env, AutoRef<Scope> scope, const _VAR & var) {
		if (var->isFunction()) {
			return var;
		}
		if (var->isSymbol()) {
			return scope->rget_func(var->r_symbol());
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
			} else if (*iter == '\'' || *iter == '`') {
				tokens.push_back(string(1, *iter));
			} else if (*iter == ',') {
				if (((iter + 1) != s.end()) && *(iter + 1) == '@') {
					iter++;
					tokens.push_back(",@");
				} else {
					tokens.push_back(string(1, *iter));
				}
			} else if (*iter == '#') {
				iter++;
				if (iter == s.end()) {
					throw LispException("unexpected end of string");
				}
				switch (*iter) {
				case '\'':
					tokens.push_back("#'");
					break;
				case '\\':
					tokens.push_back("#\\");
					break;
				default:
					throw LispException("unexpected token '" + string(1, *iter) + "' after #");
				}
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
		} else if (*iter == ",@") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, ",@"));
			lst.push_back(read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "#'") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "function"));
			lst.push_back(read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "#\\") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "#\\"));
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

	static bool silentsymboleq(const _VAR & var, const string & sym) {
		return (var->isSymbol() && var->r_symbol() == sym);
	}

	static bool silentkeywordeq(const _VAR & var, const string & key) {
		return (var->isKeyword() && var->r_keyword() == key);
	}

	static _VAR quoty(Env & env, _VAR var) {
		vector<_VAR> qa;
		qa.push_back(_HEAP_ALLOC(env, "quote"));
		qa.push_back(var);
		return _HEAP_ALLOC(env, qa);
	}

	static _VAR quote(Env & env, AutoRef<Scope> scope, _VAR var) {
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
	
	static _VAR quasi(Env & env, AutoRef<Scope> scope, _VAR var) {
		if (var->isList()) {
			vector<_VAR> lst = var->r_list();
			if (lst.empty()) {
				return _NIL(env);
			}
			vector<_VAR> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				if (lst[i]->isList()) {
					vector<_VAR> lv = lst[i]->r_list();
					if (lv.empty() == false && lv[0]->isSymbol()) {
						if (lv[0]->r_symbol() == ",") {
							_CHECK_ARGS_EXACT_COUNT(lv, 2);
							ret.push_back(eval(env, scope, lv[1]));
						} else if (lv[0]->r_symbol() == ",@") {
							_CHECK_ARGS_EXACT_COUNT(lv, 2);
							vector<_VAR> llv = eval(env, scope, lv[1])->r_list();
							ret.insert(ret.end(), llv.begin(), llv.end());
						} else {
							ret.push_back(quasi(env, scope, lst[i]));
						}
					}
				} else {
					ret.push_back(quasi(env, scope, lst[i]));
				}
			}
			return _HEAP_ALLOC(env, ret);
		}
		return var;
	}

	_VAR eval(Env & env, AutoRef<Scope> scope, const _VAR & var) {
		if (var->isSymbol()) {
			return scope->rget_sym(var->r_symbol());
		} else if (var->isList() == false) {
			return var;
		} else if (var->r_list().empty()) {
			return _NIL(env);
		} else {
			vector<_VAR> & lv = var->r_list();
			_VAR & cmd = lv[0];
			vector<_VAR> args(lv.begin() + 1, lv.end());
			if (silentsymboleq(cmd, "quit")) {
				throw ExitLispException((args.size() > 0 ? (int)_INT(eval(env, scope, args[0])) : 0));
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
		builtin_essential(env);
		builtin_type(env);
		builtin_algorithm(env);
		builtin_list(env);
		builtin_logic(env);
		builtin_character(env);
		builtin_string(env);
		builtin_arithmetic(env);
		builtin_mathematic(env);
		builtin_io(env);
		builtin_pathname(env);
		builtin_file(env);
		builtin_socket(env);
		builtin_concurrent(env);
		builtin_system(env);
		builtin_date(env);
		builtin_macro(env);
		builtin_db(env);
	}

	void builtin_essential(Env & env) {
		DECL_NATIVE_BEGIN(env, "eval");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return eval(env, scope, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "boundp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			if (scope->rsearch_sym(eval(env, scope, args[0])->r_symbol()).nil()) {
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "fboundp");
		{
			// TODO: to global scope
			_CHECK_ARGS_MIN_COUNT(args, 1);
			if (scope->rsearch_func(eval(env, scope, args[0])->r_symbol()).nil()) {
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "lambda");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			args[0]->typeCheck(Var::LIST);
			Func func(args[0], args[1]);
			func.closure_scope()->registries() = scope->registries();
			return _HEAP_ALLOC(env, func);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "defun");
		{
			/*
			 * desc: delcare function
			 */
			_CHECK_ARGS_MIN_COUNT(args, 3);
			args[1]->typeCheck(Var::LIST);
			_VAR lambda_list = args[1];
			_VAR doc;
			vector<_VAR> form;
			form.push_back(_HEAP_ALLOC(env, "progn"));
			size_t from = 0;
			if (args[2]->getType() == Var::STRING) {
				_CHECK_ARGS_MIN_COUNT(args, 4);
				doc = args[2];
				from = 3;
			} else {
				doc = _NIL(env);
				from = 2;
			}
			for (size_t i = from; i < args.size(); i++) {
				form.push_back(args[i]);
			}
			// vector<_VAR> progn_form;
			// progn_form.push_back(_HEAP_ALLOC(env, "progn"));
			// progn_form.push_back(_HEAP_ALLOC(env, form));
			return scope->rput_func(args[0]->r_symbol(),
									_HEAP_ALLOC(env, Func(doc, lambda_list, _HEAP_ALLOC(env, form))));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "defparameter");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			scope->rput_sym(args[0]->r_symbol(), eval(env, scope, args[1]));
			return args[0];
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "defvar");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_CHECK_ARGS_MAX_COUNT(args, 2);
			if (args.size() == 2 && scope->rsearch_sym(args[0]->r_symbol()).nil() == true) {
				scope->rput_sym(args[0]->r_symbol(), eval(env, scope, args[1]));
			}
			return args[0];
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "setf");
		{
			_CHECK_ARGS_EVEN_COUNT(args);
			_VAR ret = _NIL(env);
			_FORI_STEP(args, i, 0, 2) {
				_VAR a = eval(env, scope, args[i + 0]);
				_VAR b = eval(env, scope, args[i + 1]);
				if (a->isList() && b->isList()) {
					for (size_t j = 0; j < a->r_list().size() && j < b->r_list().size(); j++) {
						(*a->r_list()[j]) = (*b->r_list()[j]);
					}
				} else {
					*a = *b;
				}
				ret = a;
			}
			return ret;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "setq");
		{
			_CHECK_ARGS_EVEN_COUNT(args);
			_VAR ret = _NIL(env);
			_FORI_STEP(args, i, 0, 2) {
				ret = scope->rput_sym(args[i + 0]->r_symbol(), eval(env, scope, args[i + 1]));
			}
			return ret;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "#\\");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, Character(args[0]->r_symbol()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "quote");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return quote(env, scope, args[0]);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "`");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return quasi(env, scope, args[0]);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "function");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return function(env, scope, args[0]);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "funcall");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR funcsym = eval(env, scope, args[0]);
			_VAR func = function(env, scope, funcsym);
			vector<_VAR> fargs(args.begin() + 1, args.end());
			return func->proc(env, scope, fargs);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "let");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret = _NIL(env);
			vector<_VAR> & lets = args[0]->r_list();
			AutoRef<Scope> local_scope(new Scope);
			local_scope->parent() = scope;
			for (vector<_VAR>::iterator iter = lets.begin(); iter != lets.end(); iter++) {
				vector<_VAR> decl = (*iter)->r_list();
				string sym = decl[0]->r_symbol();
				local_scope->put_sym(sym, eval(env, scope, decl[1]));
			}
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				ret = eval(env, local_scope, *iter);
			}
			return ret;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "if");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR val = eval(env, scope, args[0]);
			if (!val->isNil()) {
				return eval(env, scope, args[1]);
			} else if (args.size() > 2) {
				return eval(env, scope, args[2]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "when");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			if (!test->isNil()) {
				return eval(env, scope, args[1]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "unless");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			if (test->isNil()) {
				return eval(env, scope, args[1]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "cond");
		{
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				vector<_VAR> lst = (*iter)->r_list();
				if (!eval(env, scope, lst[0])->isNil()) {
					return eval(env, scope, lst[1]);
				}
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "progn");
		{
			_VAR ret = _NIL(env);
			_FORI(args, i, 0) {
				ret = eval(env, scope, args[i]);
			}
			return ret;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "while");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR pre_test = args[0];
			while (!eval(env, scope, pre_test)->isNil()) {
				eval(env, scope, args[1]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "dolist");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			vector<_VAR> decl = args[0]->r_list();
			string param = decl[0]->r_symbol();
			vector<_VAR> lst = eval(env, scope, decl[1])->r_list();
			AutoRef<Scope> local_scope(new Scope);
			local_scope->parent() = scope;
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
				local_scope->put_sym(param, *iter);
				eval(env, local_scope, args[1]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "dotimes");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			vector<_VAR> steps = args[0]->r_list();
			string sym = steps[0]->r_symbol();
			Integer limit = eval(env, scope, steps[1])->r_integer();
			AutoRef<Scope> local_scope(new Scope);
			local_scope->parent() = scope;
			local_scope->put_sym(sym, _HEAP_ALLOC(env, Integer(0)));
			for (; local_scope->get_sym(sym)->r_integer() < limit;
				 local_scope->put_sym(sym, _HEAP_ALLOC(env, local_scope->get_sym(sym)->r_integer() + 1))) {
				eval(env, local_scope, args[1]);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "loop");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "list");
		{
			vector<_VAR> elts;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				elts.push_back(eval(env, scope, *iter));
			}
			return _HEAP_ALLOC(env, elts);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "cons");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "car");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			vector<_VAR> & lst = eval(env, scope, args[0])->r_list();
			if (lst.size() > 0) {
				return lst[0];
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "cdr");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "nth");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			size_t idx = (size_t)_INT(eval(env, scope, args[0]));
			vector<_VAR> & lst = eval(env, scope, args[1])->r_list();
			if (idx < lst.size()) {
				return lst[idx];
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "nthcdr");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			size_t idx = (size_t)_INT(eval(env, scope, args[0]));
			vector<_VAR> & lst = eval(env, scope, args[1])->r_list();
			if (idx < lst.size()) {
				vector<_VAR> rest;
				for (vector<_VAR>::iterator iter = lst.begin() + idx; iter != lst.end(); iter++) {
					rest.push_back(*iter);
				}
				return _HEAP_ALLOC(env, rest);
			}
			return _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "subseq");
		{
			_CHECK_ARGS_MIN_COUNT(args, 3);
			vector<_VAR> & lst = eval(env, scope, args[0])->r_list();
			Integer start = eval(env, scope, args[1])->r_integer();
			Integer end = eval(env, scope, args[2])->r_integer();
			vector<_VAR> ret;
			for (size_t i = (size_t)*start; i < (size_t)*end && i < lst.size(); i++) {
				ret.push_back(lst[i]);
			}
			return _HEAP_ALLOC(env, ret);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "unwind-protect");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "catch");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "throw");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			throw ThrowLispException(eval(env, scope, args[0]), eval(env, scope, args[1]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "block");
		{
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "return-from");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret = _OPT_EVAL(env, scope, args, 1, _NIL(env));
			throw ReturnLispException(args[0], ret);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "defmacro");
		{
			// @refer http://clhs.lisp.se/Body/m_defmac.htm
			_CHECK_ARGS_MIN_COUNT(args, 3);
			args[1]->typeCheck(Var::LIST);
			_VAR lambda_list = args[1];
			_VAR doc;
			_VAR form;
			if (args[2]->getType() == Var::STRING) {
				_CHECK_ARGS_MIN_COUNT(args, 4);
				doc = args[2];
				form = args[3];
			} else {
				doc = _NIL(env);
				form = args[2];
			}
			return scope->rput_func(args[0]->r_symbol(), _HEAP_ALLOC(env, Func(true, doc, lambda_list, form)));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "macroexpand");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			vector<_VAR> vars = eval(env, scope, args[0])->r_list();
			_CHECK_ARGS_MIN_COUNT(vars, 1);
			vector<_VAR> xargs(vars.begin() + 1, vars.end());
			return function(env, scope, vars[0])->expand(env, scope, vars[0], xargs);
		}DECL_NATIVE_END();
	}

	void builtin_type(Env & env) {
		DECL_NATIVE_BEGIN(env, "symbolp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isSymbol());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "keywordp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isKeyword());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "listp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isList());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "booleanp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isBoolean());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "integerp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isInteger());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "floatp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFloat());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "stringp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isString());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "funcp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFunction());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathnamep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFile());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "streamp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFileDescriptor());
		}DECL_NATIVE_END();
	}

	void builtin_algorithm(Env & env) {
		// TODO: refer - [http://www.lispworks.com/documentation/lw60/CLHS/Body/f_map.htm]
		DECL_NATIVE_BEGIN(env, "map");
		{
			_CHECK_ARGS_MIN_COUNT(args, 3);
			// TODO: check - http://clhs.lisp.se/Body/f_map.htm
			_VAR result_type = eval(env, scope, args[0]); /* TODO: use it */
			_VAR func = function(env, scope, eval(env, scope, args[1]));
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
					fargs.push_back(quoty(env, (i < lst.size() ? lst[i] : _NIL(env))));
				}
				_VAR r = func->proc(env, scope, fargs);
				ret.push_back(r);
			}
			return _HEAP_ALLOC(env, ret);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "mapcar");
		{
			// TODO: check - http://clhs.lisp.se/Body/f_map.htm
			_VAR func = function(env, scope, eval(env, scope, args[0]));
			vector<_VAR> ret;
			vector<vector<_VAR> > lists;
			size_t size = 0;
			for (size_t i = 1; i < args.size(); i++) {
				vector<_VAR> lst = eval(env, scope, args[i])->r_list();
				if (lst.size() > size) {
					size = lst.size();
				}
				lists.push_back(lst);
			}
			for (size_t i = 0; i < size; i++) {
				vector<_VAR> fargs;
				for (vector<vector<_VAR> >::iterator iter = lists.begin(); iter != lists.end(); iter++) {
					vector<_VAR> lst = (*iter);
					fargs.push_back(quoty(env, (i < lst.size() ? lst[i] : _NIL(env))));
				}
				_VAR r = func->proc(env, scope, fargs);
				ret.push_back(r);
			}
			return _HEAP_ALLOC(env, ret);
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "reduce");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = function(env, scope, eval(env, scope, args[0]));
			vector<_VAR> lst = eval(env, scope, args[1])->r_list();
			_VAR sum = (lst.size() > 0 ? lst[0] : _NIL(env));
			for (size_t i = 1; i < lst.size(); i++) {
				vector<_VAR> fargs;
				fargs.push_back(sum);
				fargs.push_back(lst[i]);
				sum = func->proc(env, scope, fargs);
			}
			return sum;
		}DECL_NATIVE_END();
	}

	void builtin_list(Env & env) {
		DECL_NATIVE_BEGIN(env, "length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			vector<_VAR> lst = eval(env, scope, args[0])->r_list();
			return _HEAP_ALLOC(env, Integer((long long)lst.size()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			vector<_VAR> ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				vector<_VAR> lst = eval(env, scope, *iter)->r_list();
				ret.insert(ret.end(), lst.begin(), lst.end());
			}
			return _HEAP_ALLOC(env, ret);
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "remove-if");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = function(env, scope, eval(env, scope, args[0]));
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
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "remove-if-not");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = function(env, scope, eval(env, scope, args[0]));
			vector<_VAR> lst = eval(env, scope, args[1])->r_list();
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				vector<_VAR> fargs;
				fargs.push_back(*iter);
				if (func->proc(env, scope, fargs)->isNil()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}DECL_NATIVE_END();

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
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
	}

	void builtin_character(Env & env) {
		DECL_NATIVE_BEGIN(env, "character");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "characterp");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "alpha-char-p");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "alpha-numeric-p");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "digit-char-p");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "graphic-char-p");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "standard-char-p");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "upcase");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().upcase());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "downcase");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().downcase());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "upper-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().upper_case_p());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "lower-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().lower_case_p());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "both-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().both_case_p());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-code");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().raw());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "code-char");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, Character((char)_INT(eval(env, scope, args[0]))));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-int");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, _CHAR(eval(env, scope, args[0])));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-code-limit");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "charname");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, wrap_text(eval(env, scope, args[0])->r_character().charname()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-equal");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-lessp");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char-greaterp");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch == eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char/=");
		{
            throw LispException("not implemented");
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch < eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char>");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch > eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch <= eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "char>=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch >= eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
	}

	void builtin_string(Env & env) {
		DECL_NATIVE_BEGIN(env, "string");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			switch (v->getType()) {
			case Var::STRING:
				return v;
			case Var::SYMBOL:
				return _HEAP_ALLOC(env, wrap_text(v->r_symbol()));
			case Var::CHARACTER:
				return _HEAP_ALLOC(env, wrap_text(string(1, (char)v->r_character().raw())));
			default:
				break;
			}
			throw LispException(v->toString() + " is not a string designator");
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-prefix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toString();
			string dst = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, Text::startsWith(str, dst));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-suffix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toString();
			string dst = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, Text::endsWith(str, dst));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer len((long long)eval(env, scope, args[0])->toString().length());
			return _HEAP_ALLOC(env, len);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "string-append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			string ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				ret.append(eval(env, scope, *iter)->toString());
			}
			return _HEAP_ALLOC(env, wrap_text(ret));
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "enough-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string org = eval(env, scope, args[0])->toString();
			string prefix = eval(env, scope, args[1])->toString();
			if (Text::startsWith(org, prefix)) {
				return _HEAP_ALLOC(env, wrap_text(org.substr(prefix.length())));
			}
			return _HEAP_ALLOC(env, wrap_text(org));
		}DECL_NATIVE_END();
	}
	void builtin_arithmetic(Env & env) {
		DECL_NATIVE_BEGIN(env, "+");
		{
			_VAR v = _HEAP_ALLOC(env, 0);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = plus(env, v, eval(env, scope, *iter));
			}
			return v;
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "*");
		{
			_VAR v = _HEAP_ALLOC(env, 1);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = multiply(env, v, eval(env, scope, *iter));
			}
			return v;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "/");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				v = divide(env, v, eval(env, scope, *iter));
			}
			return v;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "%");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer sum = eval(env, scope, args[0])->r_integer();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				sum %= eval(env, scope, *iter)->r_integer();
			}
			return _HEAP_ALLOC(env, sum);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!eq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, ">");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!gt(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!lt(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, ">=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!gteq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!lteq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _HEAP_ALLOC(env, true);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "oddp");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_integer().oddp());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "evenp");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_integer().evenp());
		}DECL_NATIVE_END();
	}
	void builtin_mathematic(Env & env) {
		env.scope()->put_sym("pi", _HEAP_ALLOC(env, 3.141592653589793));
		DECL_NATIVE_BEGIN(env, "cos");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _cos(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "sin");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _sin(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "tan");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _tan(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "acos");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _acos(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "asin");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _asin(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "atan");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _atan(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "abs");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _abs(env, eval(env, scope, args[0]));
		}DECL_NATIVE_END();
	}
	void builtin_io(Env & env) {

		env.scope()->put_sym("*standard-output*",
							 _HEAP_ALLOC(env, AutoRef<FileDescriptor>(new FileDescriptor(stdout, false))));
		env.scope()->put_sym("*standard-input*",
							 _HEAP_ALLOC(env, AutoRef<FileDescriptor>(new FileDescriptor(stdin, false))));

		DECL_NATIVE_BEGIN(env, "read");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret = _NIL(env);
			AutoRef<FileDescriptor> fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd->eof()) {
				return _HEAP_ALLOC(env, true);
			}
			BufferedCommandReader reader;
			while (!fd->eof() && reader.read(string(1, (char)fd->read())) < 1) {}
                
			vector<string> commands = reader.getCommands();
			for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
				ret = parse(env, *iter);
				env.gc();
			}
			return ret;

        }DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "read-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			AutoRef<FileDescriptor> fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd->eof()) {
				return _HEAP_ALLOC(env, true);
			}
			string line = fd->readline();
			return _HEAP_ALLOC(env, wrap_text(line));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "print");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			AutoRef<FileDescriptor> fd = scope->rget_sym("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd->write(msg);
			fd->write("\n");
			return _HEAP_ALLOC(env, wrap_text(msg));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "write-string");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			AutoRef<FileDescriptor> fd = scope->rget_sym("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd->write(msg);
			return _HEAP_ALLOC(env, wrap_text(msg));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "write-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			AutoRef<FileDescriptor> fd = scope->rget_sym("*standard-output*")->r_fileDescriptor();
			if (args.size() == 2) {
				fd = eval(env, scope, args[1])->r_fileDescriptor();
			}
			string msg = eval(env, scope, args[0])->toString();
			fd->write(msg);
			fd->write("\n");
			return _HEAP_ALLOC(env, wrap_text(msg));
		}DECL_NATIVE_END();
	}
	void builtin_pathname(Env & env) {
		DECL_NATIVE_BEGIN(env, "pathname");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR path = pathname(env, eval(env, scope, args[0]));
			return path;
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathname-name");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getFileNameWithoutExtension()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "pathname-type");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getExtension()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getPath()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "directory-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getDirectory()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, wrap_text(file.getFileName()));
		}DECL_NATIVE_END();
		
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
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "probe-file");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return file.exists() ? _HEAP_ALLOC(env, file) : _NIL(env);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "dirp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, file.isDirectory());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "filep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, file.isFile());
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)file.getSize()));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-attribute-creation");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.creationTime()).sec));
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "file-attribute-lastmodified");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			File file = pathname(env, eval(env, scope, args[0]))->r_file();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(file.lastModifiedTime()).sec));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "open");
		{
			Parameters params = Parameters::parse(
				env, scope, parse(env, "(fname &key (if-does-not-exist :error) (if-exists :new-version))")->r_list());
			params.bind(env, scope, scope, args, true);
			File file = pathname(env, scope->get_sym("fname"))->r_file();
			const char * flags = "rb+";
			if (file.exists() == false) {
				// does not exists
				if (scope->get_sym("if-does-not-exist")->isNil()) {
					return _NIL(env);
				} else if (silentkeywordeq(scope->get_sym("if-does-not-exist"), ":error")) {
					throw LispException("cannot open");
				} else if (silentkeywordeq(scope->get_sym("if-does-not-exist"), ":create")) {
					flags = "wb+";
				}
			} else {
				// exists
				if (scope->get_sym("if-exists")->isNil()) {
					return _NIL(env);
				} else if (silentkeywordeq(scope->get_sym("if-exists"), ":append")) {
					flags = "ab+";
				} else if (silentkeywordeq(scope->get_sym("if-exists"), ":overwrite")) {
					flags = "wb+";
				}
			}
			return _HEAP_ALLOC(env, AutoRef<FileDescriptor>(
								   new FileDescriptor(FileStream::s_open(file.getPath(), flags))));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "file-position");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			AutoRef<FileDescriptor> fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (args.size() > 1) {
				fd->position((size_t)_INT(eval(env, scope, args[1])));
			}
			return _HEAP_ALLOC(env, Integer((long long)fd->position()));
				
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "close");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			eval(env, scope, args[0])->r_fileDescriptor()->close();
			return _NIL(env);
		}DECL_NATIVE_END();
	}
	void builtin_socket(Env & env) {
		DECL_NATIVE_BEGIN(env, "server-open");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			int port = (int)_INT(eval(env, scope, args[0]));
			return _HEAP_ALLOC(env, AutoRef<LispExtension>(new LispServerSocket(port)));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "connect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			_VAR host = eval(env, scope, args[0]);
			_VAR port = eval(env, scope, args[1]);
			try {
				AutoRef<Socket> socket(new Socket(InetAddress(host->toString(), (int)_INT(port))));
				socket->connect();
				return _HEAP_ALLOC(env, AutoRef<LispExtension>(new LispSocket(socket)));
			} catch (Exception e) {
				throw LispException("socket connect exception - " + e.message());
			}
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "accept");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			_VAR serv = eval(env, scope, args[0]);
			AutoRef<LispExtension> ext = serv->r_ext();
			LispServerSocket * server = ((LispServerSocket*)&ext);
			try {
				return _HEAP_ALLOC(env, AutoRef<LispExtension>(new LispSocket(server->accept())));
			} catch (Exception e) {
				throw LispException("socket accept exception - " + e.message());
			}
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "recv");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_CHECK_ARGS_MAX_COUNT(args, 2);
			_VAR err;
			_VAR sock = eval(env, scope, args[0]);
			if (args.size() == 2) {
				err = eval(env, scope, args[1]);
			}
			AutoRef<LispExtension> ext = sock->r_ext();
			LispSocket * socket = ((LispSocket*)&ext);
			char d;
			try {
				socket->recv(&d, 1);
			} catch (Exception e) {
				if (err.nil()) {
					throw LispException("socket recv exception - " + e.message());
				}
				return err;
			}
			return _HEAP_ALLOC(env, (int)d);
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "send");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			_VAR sock = eval(env, scope, args[0]);
			_VAR data = eval(env, scope, args[1]);
			AutoRef<LispExtension> ext = sock->r_ext();
			LispSocket * socket = ((LispSocket*)&ext);
			int cnt = 0;
			try {
				if (data->isList()) {
					vector<_VAR> lst = data->r_list();
					_FORI(lst, i, 0) {
						cnt += socket->send(lst[i]);
					}
				} else {
					cnt = socket->send(data);
				}
				return _HEAP_ALLOC(env, cnt);
			} catch (Exception e) {
				throw LispException("socket send exception - " + e.message());
			}
		}DECL_NATIVE_END();
	}
	void builtin_concurrent(Env & env) {
		// todo: fill it
	}
	void builtin_system(Env & env) {
		DECL_NATIVE_BEGIN(env, "system-type");
		{
#if defined(PLATFORM_APPLE)
			return _HEAP_ALLOC(env, "apple");
#elif defined(PLATFORM_WINDOWS)
			return _HEAP_ALLOC(env, "windows");
#elif defined(PLATFORM_UNIX)
			return _HEAP_ALLOC(env, "unix");
#else
			throw LispException("unknown system type");
#endif
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "system");
		{
#if defined(USE_APPLE_STD)
            throw LispException("system() deprecated");
#else
            _CHECK_ARGS_MIN_COUNT(args, 1);
			Integer ret(system(eval(env, scope, args[0])->toString().c_str()));
            return _HEAP_ALLOC(env, ret);
#endif
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "run-process");
		{
            _CHECK_ARGS_MIN_COUNT(args, 1);
			Process p(eval(env, scope, args[0])->toString());
			p.start();
			FileStream out(p.out());
			string o;
			while (!out.eof()) {
				o.append(out.readline());
				if (out.eof()) {
					break;
				}
				o.append("\n");
			}
			p.wait();
			p.close();
            return _HEAP_ALLOC(env, wrap_text(o));
		}DECL_NATIVE_END();		
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
		}DECL_NATIVE_END();
	}
	
	void builtin_date(Env & env) {
		env.scope()->put_sym("internal-time-units-per-second", _HEAP_ALLOC(env, 1000));
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
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "get-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			return _HEAP_ALLOC(env, (long long)osl_get_time_network().sec);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "decode-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			// TODO: apply specific zone - eval args[1]
			// TODO: (decode-universal-time 0 0) - expect 0, 0, 0, 1, 1, 1900, 0, false, 0
			osl_time_t time = {0,};
			time.sec = (unsigned long)_INT(eval(env, scope, args[0]));
			time = osl_network_time_to_system_time(time);
			Date date(time);
			vector<_VAR> ret;
			ret.push_back(_HEAP_ALLOC(env, date.getSecond()));
			ret.push_back(_HEAP_ALLOC(env, date.getMinute()));
			ret.push_back(_HEAP_ALLOC(env, date.getHour()));
			ret.push_back(_HEAP_ALLOC(env, date.getDay()));
			ret.push_back(_HEAP_ALLOC(env, date.getMonth() + 1));
			ret.push_back(_HEAP_ALLOC(env, date.getYear()));
			ret.push_back(_HEAP_ALLOC(env, date.getDayOfWeek()));
			ret.push_back(_NIL(env)); // TODO: daylight-p ???
			ret.push_back(_HEAP_ALLOC(env, -date.getGmtOffset())); // minute offset
			return _HEAP_ALLOC(env, ret);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "encode-universal-time");
		{
			// Arguments: seconds, minutes, hours, dates, month and year (gmt offset)
			// TODO: (encode-universal-time 0 0 0 1 1 1900 0) -> expect 0
			_CHECK_ARGS_MIN_COUNT(args, 6);
			Date date = Date::now();
			date.setSecond((int)_INT(eval(env, scope, args[0])));
			date.setMinute((int)_INT(eval(env, scope, args[1])));
			date.setHour((int)_INT(eval(env, scope, args[2])));
			date.setDay((int)_INT(eval(env, scope, args[3])));
			date.setMonth((int)((_INT(eval(env, scope, args[4]))) - 1));
			date.setYear((int)_INT(eval(env, scope, args[5])));
			if (args.size() > 6) {
				date.setGmtOffset((int)_INT(eval(env, scope, args[6])));
			}
			return _HEAP_ALLOC(env, (long long)osl_system_time_to_network_time(date.getTime()).sec);
		}DECL_NATIVE_END();
		DECL_NATIVE_BEGIN(env, "format-time-string-rfc8601");
		{
			// Arguments: seconds, minutes, hours, dates, month and year (gmt offset)
			_CHECK_ARGS_MIN_COUNT(args, 6);
			Date date = Date::now();
			date.setSecond((int)_INT(eval(env, scope, args[0])));
			date.setMinute((int)_INT(eval(env, scope, args[1])));
			date.setHour((int)_INT(eval(env, scope, args[2])));
			date.setDay((int)_INT(eval(env, scope, args[3])));
			date.setMonth((int)((_INT(eval(env, scope, args[4]))) - 1));
			date.setYear((int)_INT(eval(env, scope, args[5])));
			if (args.size() > 6) {
				date.setGmtOffset((int)_INT(eval(env, scope, args[6])));
			}
			return _HEAP_ALLOC(env, wrap_text(Date::formatRfc8601(date)));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "format-time-string-rfc1123");
		{
			// Arguments: seconds, minutes, hours, dates, month and year (gmt offset)
			_CHECK_ARGS_MIN_COUNT(args, 6);
			Date date = Date::now();
			date.setSecond((int)_INT(eval(env, scope, args[0])));
			date.setMinute((int)_INT(eval(env, scope, args[1])));
			date.setHour((int)_INT(eval(env, scope, args[2])));
			date.setDay((int)_INT(eval(env, scope, args[3])));
			date.setMonth((int)((_INT(eval(env, scope, args[4]))) - 1));
			date.setYear((int)_INT(eval(env, scope, args[5])));
			if (args.size() > 6) {
				date.setGmtOffset((int)_INT(eval(env, scope, args[6])));
			}
			return _HEAP_ALLOC(env, wrap_text(Date::formatRfc1123(date)));
		}DECL_NATIVE_END();
	}

	void builtin_macro(Env & env) {
		compile(env, "(defmacro incf (x &optional (i 1)) `(setf ,x (+ ,x ,i)))");
		compile(env, "(defmacro decf (x &optional (i 1)) `(setf ,x (- ,x ,i)))");
		compile(env, "(defmacro 1+ (x) `(+ ,x 1))");
		compile(env, "(defmacro 1- (x) `(- ,x 1))");
		compile(env, "(defmacro cadr (x) `(car (cdr ,x)))");
	}

	void builtin_db(Env & env) {
		DECL_NATIVE_BEGIN(env, "db:load");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			string path = eval(env, scope, args[0])->toString();
			string name = eval(env, scope, args[1])->toString();
			DatabaseDriver::instance().load(name, AutoRef<Library>(new Library(path, name)));
			return _HEAP_ALLOC(env, name);
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:connect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 6);
			string name = eval(env, scope, args[0])->toString();
			string hostname = eval(env, scope, args[1])->toString();
			int port = (int)_INT(eval(env, scope, args[2]));
			string username = eval(env, scope, args[3])->toString();
			string password = eval(env, scope, args[4])->toString();
			string dbname = eval(env, scope, args[5])->toString();
			LispDatabaseConnection * conn = new LispDatabaseConnection(DatabaseDriver::instance().getConnection(name));
			conn->connect(hostname, port, username, password, dbname);
			return _HEAP_ALLOC(env, AutoRef<LispExtension>(conn));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:disconnect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_ext());
			conn->disconnect();
			return _NIL(env);
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:query");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_ext());
			string sql = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, AutoRef<LispExtension>(new LispResultSet(conn->query(sql))));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:fetch");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			LispResultSet * resultSet = ((LispResultSet *)&eval(env, scope, args[0])->r_ext());
			if (resultSet->next() == false) {
				return _NIL(env);
			}
			vector<_VAR> row;
			for (int i = 0; i < resultSet->fieldCount(); i++) {
				row.push_back(_HEAP_ALLOC(env, wrap_text(resultSet->getString(i))));
			}
			return _HEAP_ALLOC(env, row);
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:update");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_ext());
			string sql = eval(env, scope, args[1])->toString();
			return _HEAP_ALLOC(env, conn->queryUpdate(sql));
		}DECL_NATIVE_END();

		DECL_NATIVE_BEGIN(env, "db:escape");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			string sql = eval(env, scope, args[0])->toString();
			return _HEAP_ALLOC(env, wrap_text(Text::replaceAll(sql, "'", "\\'")));
		}DECL_NATIVE_END();
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

	static string printVar(_VAR var) {
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
