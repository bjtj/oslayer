#include <cmath>
#include "Lisp.hpp"
#include "os.hpp"
#include "Text.hpp"
#include "FileStream.hpp"
#include "Socket.hpp"
#include "AutoLock.hpp"
#include "Process.hpp"
#include "DatabaseDriver.hpp"
#include "DatabaseConnection.hpp"
#include "Timer.hpp"

#define _HEAP_ALLOC(E,...) E.alloc(new Var(__VA_ARGS__))
#define _VAR GCRef<Var>
#define _NIL(E) E.scope()->rget_const(Symbol("!nil"))
#define _TRUE(E) E.scope()->rget_const(Symbol("!t"))
#define _NIL_OR_PASS(E,V) ((V).nil() ? _NIL(E) : (V))
#define _SAFE_STRING(V) ((V).nil() ? "(null)" : (V)->toString())
#define _CHECK_ARGS_MIN_COUNT(L,C)										\
  do {																	\
	  if ((L).size() < C) {												\
		  throw LispException("Wrong argument count: " + Text::toString((L).size()) \
							  + " / expected minimum: " + Text::toString(C)); \
	  }																	\
  } while(0);
#define _CHECK_ARGS_EXACT_COUNT(L,C)									\
  do {																	\
	  if ((L).size() != C) {											\
		  throw LispException("Wrong argument count: " + Text::toString((L).size()) \
							  + " / expected: " + Text::toString(C));	\
	  }																	\
  } while(0);
#define _CHECK_ARGS_MAX_COUNT(L,C)										\
  do {																	\
	  if ((L).size() > C) {												\
		  throw LispException("Wrong argument count: " + Text::toString((L).size()) \
							  + " / expected maximum: " + Text::toString(C)); \
	  }																	\
  } while(0);
#define _CHECK_ARGS_EVEN_COUNT(L)										\
  do {																	\
	  if ((L).size() % 2 != 0) {										\
		  throw LispException("Wrong argument count: " + Text::toString((L).size()) \
							  + " / expected : even");					\
	  }																	\
  } while(0);

#define _EQ_NIL_OR_SYMBOL(A,B) (((A)->isNil() && (B)->isNil()) ||		\
								(((A)->isNil() == false && (B)->isNil() == false) && \
								 ((A)->r_symbol() == (B)->r_symbol())))
#define _OPT_EVAL(E,S,L,N,D) (N < L.size() ? eval(E, S, L[N]) : D)
#define _FORI(L,I,F) for (size_t I = (F); I < (L).size(); I++)
#define _FORI_STEP(L,I,F,S) for (size_t I = (F); I < (L).size(); I += (S))

#define _CHAR(V) (V->r_character().raw())
#define _INT(V) (V->r_integer().raw())
#define _FLOAT(V) (V->r_float().raw())

#define BEGIN_DECL_NATIVE(ENV,NAME)								\
	do {														\
	Env & _E = ENV;												\
	string _N = NAME;											\
	class _cls : public Procedure {								\
	private:														\
	public:															\
	_cls() : Procedure(NAME) {}										\
	virtual ~_cls() {}												\
	LISP_PROCEDURE_PROC(env, scope, name, args) {
#define END_DECL_NATIVE												\
	}																\
	};																\
	_E.scope()->put_func(_N, _E.alloc(new Var(new _cls)));			\
	} while (0)

namespace LISP {

	using namespace std;
	using namespace OS;
	using namespace UTIL;

	inline static string _to_string(_VAR var) {
		if (var.nil()) {
			return "(undefined)";
		}
		return var->toString();
	}

	/**
	 * @brief native exception wrapper
	 */

	NativeLispException::NativeLispException(Exception & e)
		: _e(e) {
	}
	NativeLispException::~NativeLispException() throw() {
	}
	string NativeLispException::toString() const {
		return "{NATIVE EXCEPTION: '" + _e.message() + "'}";
	}

	/**
	 * @brief exit lisp exception
	 */

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
	 * @brief
	 */

	inline static string _reg_id_to_string(const REG_ID & id) {
		switch (id) {
		case REG_VARIABLE:
			return "REG_VARIABLE";
		case REG_CONST:
			return "REG_CONST";
		case REG_FUNCTION:
			return "REG_FUNCTION";
		default:
			break;
		}
		return "(UNKNOWN)";
	}

	/**
	 * @brief registry
	 */

	Registry::Registry() {
	}

	Registry::~Registry() {
	}

	bool Registry::contains(const Symbol & k) {
		return (find(k) != end() ? true : false);
	}
	
	/**
	 * @brief scope
	 */

	Scope::Scope() {
	}

	Scope::Scope(UnsafeAutoRef<Scope> parent) : _parent(parent) {
	}
	
	Scope::~Scope() {
	}
	
	UnsafeAutoRef<Scope> & Scope::parent() {
		return _parent;
	}
	void Scope::clear() {
		_registries.clear();
	}
	
	map<REG_ID, Registry> & Scope::registries() {
		return _registries;
	}
	
	Registry & Scope::registry(const REG_ID & id) {
		return _registries[id];
	}
	
	// var
	_VAR Scope::search_var(const Symbol & sym) {
		return search(REG_VARIABLE, sym);
	}
	
	_VAR Scope::rsearch_var(const Symbol & sym) {
		return rsearch(REG_VARIABLE, sym);
	}
	
	_VAR Scope::rget_var(const Symbol & sym) {
		return rget(REG_VARIABLE, sym);
	}
	
	_VAR Scope::rput_var(const Symbol & sym, const _VAR & var) {
		return rput(REG_VARIABLE, sym, var);
	}

	// const
	_VAR Scope::search_const(const Symbol & sym) {
		return search(REG_CONST, sym);
	}
	
	_VAR Scope::rsearch_const(const Symbol & sym) {
		return rsearch(REG_CONST, sym);
	}
	
	_VAR Scope::rget_const(const Symbol & sym) {
		return rget(REG_CONST, sym);
	}
	
	_VAR Scope::rput_const(const Symbol & sym, const _VAR & var) {
		return rput(REG_CONST, sym, var);
	}

	// func
	_VAR Scope::search_func(const Symbol & sym) {
		return search(REG_FUNCTION, sym);
	}
	
	_VAR Scope::rsearch_func(const Symbol & sym) {
		return rsearch(REG_FUNCTION, sym);
	}

	_VAR Scope::rget_func(const Symbol & sym) {
		return rget(REG_FUNCTION, sym);
	}

	_VAR Scope::rget_func(Symbol & sym) {
		return rget(REG_FUNCTION, sym);
	}

	_VAR Scope::rput_func(const Symbol & sym, const _VAR & var) {
		return rput(REG_FUNCTION, sym, var);
	}

	_VAR Scope::search(const REG_ID & id, const Symbol & sym) {
		if (registry(id).contains(sym)) {
			return registry(id)[sym];
		}
		return _VAR();
	}

	_VAR Scope::rsearch(const REG_ID & id, const Symbol & sym) {
		if (registry(id).contains(sym)) {
			return registry(id)[sym];
		}
		if (_parent.nil() == false) {
			return _parent->rsearch(id, sym);
		}
		return _VAR();
	}

	_VAR Scope::rget(const REG_ID & id, const Symbol & sym) {
		if (registry(id).contains(sym)) {
			return registry(id)[sym];
		}
		if (_parent.nil() == false) {
			return _parent->rget(id, sym);
		}
		throw UnboundLispException(sym.symbol());
	}
	_VAR Scope::rput(const REG_ID & id, const Symbol & sym, const _VAR & var) {
		if (registry(id).contains(sym)) {
			registry(id)[sym] = var;
			return var;
		}
		if (_parent.nil() == false) {
			return _parent->rput(id, sym, var);
		}
		registry(id)[sym] = var;
		return var;
	}

	// var
	_VAR Scope::get_var(const Symbol & sym) {
		return get(REG_VARIABLE, sym);
	}
	
	void Scope::put_var(const Symbol & sym, const _VAR & var) {
		put(REG_VARIABLE, sym, var);
	}

	// const
	_VAR Scope::get_const(const Symbol & sym) {
		return get(REG_CONST, sym);
	}
	
	void Scope::put_const(const Symbol & sym, const _VAR & var) {
		put(REG_CONST, sym, var);
	}

	// func
	_VAR Scope::get_func(const Symbol & sym) {
		return get(REG_FUNCTION, sym);
	}
	
	void Scope::put_func(const Symbol & sym, const _VAR & var) {
		put(REG_FUNCTION, sym, var);
	}
	
	_VAR Scope::get(const REG_ID & id, const Symbol & sym) {
		if (registry(id).contains(sym) == false) {
			throw UnboundLispException(sym.symbol());
		}
		return registry(id)[sym];
	}
	
	void Scope::put(const REG_ID & id, const Symbol & sym, const _VAR & var) {
		registry(id)[sym] = var;
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
		
		for (map<REG_ID, Registry>::const_iterator ri = _registries.begin(); ri != _registries.end(); ri++) {
			ret.append("$$");
			ret.append(_reg_id_to_string(ri->first));
			ret.append(": \n");
			ret.append(indent);
			ret.append("[");
			for (map<Symbol, _VAR>::const_iterator it = ri->second.begin(); it != ri->second.end(); it++) {
				if (it != ri->second.begin()) {
					ret.append("\n");
				}
				ret.append(indent);
				ret.append("'");
				ret.append(it->first.symbol());
				ret.append("': '");
				ret.append(_to_string(it->second));
				ret.append("'");
			}
			ret.append(indent);
			ret.append("]\n");
		}
		return ret;
	}

	/**
	 * @brief Sequence
	 */

	Sequence::Sequence() {
	}
	Sequence::Sequence(const vector<_VAR> & lst)
		: _lst(lst) {
	}
	Sequence::Sequence(vector<_VAR>::iterator begin, vector<_VAR>::iterator end)
		: _lst(begin, end) {
	}
	Sequence::Sequence(vector<_VAR>::const_iterator begin, vector<_VAR>::const_iterator end)
		: _lst(begin, end) {
	}
	Sequence::~Sequence() {
	}
	Iterator<_VAR> Sequence::iter() {
		return Iterator<_VAR>(_lst);
	}
	vector<_VAR> & Sequence::vec() {
		return _lst;
	}
	bool Sequence::empty() const {
		return _lst.empty();
	}
	vector<_VAR>::iterator Sequence::begin() {
		return _lst.begin();
	}
	vector<_VAR>::iterator Sequence::end() {
		return _lst.end();
	}
	vector<_VAR>::const_iterator Sequence::begin() const {
		return _lst.begin();
	}
	vector<_VAR>::const_iterator Sequence::end() const {
		return _lst.end();
	}
	size_t Sequence::size() const {
		return _lst.size();
	}
	vector<_VAR>::iterator Sequence::erase(vector<_VAR>::iterator iter) {
		return _lst.erase(iter);
	}
	void Sequence::push_back(const _VAR & var) {
		_lst.push_back(var);
	}
	void Sequence::testIndexValid(const size_t & idx) const {
		if (idx >= _lst.size()) {
			throw LispException("not allowed index: " + Text::toString(idx) +
								", size: " + Text::toString(size()));
		}
	}
	void Sequence::swap(const size_t & from, const size_t & to) {
		iter_swap(_lst.begin() + from, _lst.begin() + to);
	}
	Sequence Sequence::subseq(const size_t & start, const size_t & end) const {
		testIndexValid(start);
		testIndexValid(end);
		return Sequence(begin() + start, begin() + end);
	}
	_VAR & Sequence::operator[] (const size_t & idx) {
		testIndexValid(idx);
		return _lst[idx];
	}
	const _VAR & Sequence::operator[] (const size_t & idx) const {
		testIndexValid(idx);
		return _lst[idx];
	}
	string Sequence::toString() const {
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

	/**
	 * @brief Symbol
	 */

	Symbol::Symbol() {
	}
	Symbol::Symbol(const string & symbol) : _symbol(symbol) {
	}
	Symbol::~Symbol() {
	}
	string & Symbol::symbol() {
		return _symbol;
	}
	string Symbol::symbol() const {
		return _symbol;
	}
	bool Symbol::operator== (const string & other) const {
		return (_symbol == other);
	}
	bool Symbol::operator== (const Symbol & other) const {
		return (_symbol == other._symbol);
	}
	bool Symbol::operator< (const Symbol & other) const {
		return (_symbol < other._symbol);
	}
	bool Symbol::operator> (const Symbol & other) const {
		return (_symbol > other._symbol);
	}
	string Symbol::toString() const {
		return _symbol;
	}
	bool operator== (const std::string & a, const Symbol & b) {
		return b == a;
	}

	/**
	 * keyword
	 */

	Keyword::Keyword() {
	}
	Keyword::Keyword(const string & keyword) : Symbol(keyword) {
		if (Text::startsWith(keyword, ":") == false) {
			throw LispException("Wrong keyword format");
		}
	}
	Keyword::~Keyword() {
	}
	string & Keyword::keyword() {
		return symbol();
	}
	string Keyword::keyword() const {
		return symbol();
	}
	string Keyword::keyword_without_token() const {
		return symbol().substr(1);
	}
	Symbol Keyword::toSymbol() const {
		return Symbol(keyword_without_token());
	}
	Keyword Keyword::wrap(const Symbol & sym) {
		return Keyword(":" + sym.symbol());
	}
	string Keyword::toString() const {
		return Symbol::toString();;
	}
	bool operator== (const std::string & a, const Keyword & b) {
		return b == a;
	}

	/**
	 * boolean
	 */
	
	Boolean::Boolean() : _val(false) {
	}
	Boolean::Boolean(bool val) : _val(val) {
	}
	Boolean::~Boolean() {
	}
	bool & Boolean::val() {
		return _val;
	}
	bool Boolean::val() const {
		return _val;
	}
	bool & Boolean::operator* () {
		return _val;
	}
	Boolean & Boolean::operator= (bool val) {
		_val = val;
		return *this;
	}
	Boolean & Boolean::operator= (const Boolean & other) {
		_val = other._val;
		return *this;
	}
	bool Boolean::operator== (const bool & other) const {
		return (_val == other);
	}
	bool Boolean::operator== (const Boolean & other) const {
		return (_val == other._val);
	}
	bool Boolean::operator!= (const bool & other) const {
		return (_val != other);
	}
	bool Boolean::operator!= (const Boolean & other) const {
		return (_val != other._val);
	}
	string Boolean::toString() const {
		return (_val ? "T" : "NIL");
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
		// TODO: implement
		return false;
	}
	bool Character::standard_char_p() const {
		// TODO: implement
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
		// TODO: implement
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
	string Character::toPrintString() const {
		return string(1, (char)_ch);
	}
	string Character::toString() const {
		return "#\\" + string(1, (char)_ch);
	}

	/**
	 * 
	 */

	Number::Number() {
	}
	Number::~Number() {
	}

	/**
	 * 
	 */
	
	Integer::Integer() : num(0) {
	}
	Integer::Integer(short num) : num(num) {
	}
	Integer::Integer(int num) : num(num) {
	}
	Integer::Integer(long num) : num(num) {
	}
	Integer::Integer(long long num) : num(num) {
	}
	Integer::~Integer() {
	}

	bool Integer::isIntegerString(const string & istr) {
		size_t f = 0;
		if (*istr.begin() == '-' || *istr.begin() == '+') {
			f = 1;
		}
		return (istr.length() - f > 0) &&
			(istr.find_first_not_of("0123456789", f) == string::npos);
	}

	long long Integer::toInteger(const string & istr) {
		size_t f = (*istr.begin() == '-' || *istr.begin() == '+') ? 1 : 0;
		bool negative = (*istr.begin() == '-');
		long long n = 0;
		for (size_t i = f; i < istr.length(); i++) {
			n *= 10;
			n += istr[i] - '0';
		}
		return (negative ? -n : n);
	}
	bool Integer::zero_p() const {
		return num == 0; 
	}
	bool Integer::odd_p() const {
		// https://stackoverflow.com/a/5700938/5676460
		return ((num & 1) == 1);
	}
	bool Integer::even_p() const {
		return ((num & 1) == 0);
	}
	long long Integer::raw() const {
		return num;
	}
	long long & Integer::operator* () {
		return num;
	}
	long long Integer::getInteger() const {
		return num;
	}
	Integer & Integer::operator+= (const Integer & other) {
		num += other.num;
		return *this;
	}
	Integer & Integer::operator-= (const Integer & other) {
		num -= other.num;
		return *this;
	}
	Integer & Integer::operator*= (const Integer & other) {
		num *= other.num;
		return *this;
	}
	Integer & Integer::operator/= (const Integer & other) {
		num /= other.num;
		return *this;
	}
	Integer & Integer::operator%= (const Integer & other) {
		num %= other.num;
		return *this;
	}
	Integer Integer::operator+ (const Integer & other) const {
		return Integer(num + other.num);
	}
	Integer Integer::operator- (const Integer & other) const {
		return Integer(num - other.num);
	}
	Integer Integer::operator* (const Integer & other) const {
		return Integer(num * other.num);
	}
	Integer Integer::operator/ (const Integer & other) const {
		return Integer(num / other.num);
	}
	Integer Integer::operator% (const Integer & other) const {
		return Integer(num % other.num);
	}
	bool Integer::operator> (const Integer & other) const {
		return num > other.num;
	}
	bool Integer::operator< (const Integer & other) const {
		return num < other.num;
	}
	bool Integer::operator>= (const Integer & other) const {
		return num >= other.num;
	}
	bool Integer::operator<= (const Integer & other) const {
		return num <= other.num;
	}
	bool Integer::operator== (const Integer & other) const {
		return num == other.num;
	}
	bool Integer::operator!= (const Integer & other) const {
		return num != other.num;
	}
	Float Integer::operator+ (const Float & other) const {
		return Float((double)num) + other;
	}
	Float Integer::operator- (const Float & other) const {
		return Float((double)num) - other;
	}
	Float Integer::operator* (const Float & other) const {
		return Float((double)num) * other;
	}
	Float Integer::operator/ (const Float & other) const {
		return Float((double)num) / other;
	}
	bool Integer::operator> (const Float & other) const {
		return Float((double)num) > other;
	}
	bool Integer::operator< (const Float & other) const {
		return Float((double)num) < other;
	}
	bool Integer::operator>= (const Float & other) const {
		return Float((double)num) >= other;
	}
	bool Integer::operator<= (const Float & other) const {
		return Float((double)num) <= other;
	}
	bool Integer::operator== (const Float & other) const {
		return Float((double)num) == other;
	}
	bool Integer::operator!= (const Float & other) const {
		return Float((double)num) != other;
	}
	string Integer::toString() const {
		return Text::toString(num);
	}

	/**
	 * 
	 */

	Float::Float() : num(0.f) {
	}
	Float::Float(float num) : num((double)num) {
	}
	Float::Float(double num) : num(num) {
	}
	Float::Float(const Integer & inum) : num((double)inum.raw()) {
	}
	Float::~Float() {
	}
	bool Float::isFloatString(const string & istr) {
		string n = istr;
		if (*istr.begin() == '-' || *istr.begin() == '+') {
			n = n.substr(1);
		}
		if (n.find_first_not_of("0123456789.") != string::npos) {
			return false;
		}
		if (n.find(".") == string::npos) {
			return false;
		}
		if (n.find(".", n.find(".") + 1) != string::npos) {
			return false;
		}
		return true;
	}
	double Float::toFloat(const string & istr) {
		return (double)atof(istr.c_str());
	}
	bool Float::zero_p() const {
		return num == 0; 
	}
	double Float::raw() const {
		return num;
	}
	double & Float::operator* () {
		return num;
	}
	Float & Float::operator+= (const Float & other) {
		num += other.num;
		return *this;
	}
	Float & Float::operator-= (const Float & other) {
		num -= other.num;
		return *this;
	}
	Float & Float::operator*= (const Float & other) {
		num *= other.num;
		return *this;
	}
	Float & Float::operator/= (const Float & other) {
		num /= other.num;
		return *this;
	}
	Float Float::operator+ (const Float & other) const {
		return Float(num + other.num);
	}
	Float Float::operator- (const Float & other) const {
		return Float(num - other.num);
	}
	Float Float::operator* (const Float & other) const {
		return Float(num * other.num);
	}
	Float Float::operator/ (const Float & other) const {
		return Float(num / other.num);
	}
	bool Float::operator> (const Float & other) const {
		return num > other.num;
	}
	bool Float::operator< (const Float & other) const {
		return num < other.num;
	}
	bool Float::operator>= (const Float & other) const {
		return num >= other.num;
	}
	bool Float::operator<= (const Float & other) const {
		return num <= other.num;
	}
	bool Float::operator== (const Float & other) const {
		return num == other.num;
	}
	bool Float::operator!= (const Float & other) const {
		return num != other.num;
	}
	Float Float::operator+ (const Integer & other) const {
		return *this + Float(other);
	}
	Float Float::operator- (const Integer & other) const {
		return *this - Float(other);
	}
	Float Float::operator* (const Integer & other) const {
		return *this * Float(other);
	}
	Float Float::operator/ (const Integer & other) const {
		return *this / Float(other);
	}
	bool Float::operator> (const Integer & other) const {
		return *this > Float(other);
	}
	bool Float::operator< (const Integer & other) const {
		return *this < Float(other);
	}
	bool Float::operator>= (const Integer & other) const {
		return *this >= Float(other);
	}
	bool Float::operator<= (const Integer & other) const {
		return *this <= Float(other);
	}
	bool Float::operator== (const Integer & other) const {
		return *this == Float(other);
	}
	bool Float::operator!= (const Integer & other) const {
		return *this != Float(other);
	}
	string Float::toString() const {
		string x = Text::rtrim(Text::toString(num), "0");
		return (x[x.size() - 1] == '.' ? x + "0" : x);
	}

	/**
	 * @brief string
	 */
	String::String() {
	}
	String::String(const string & str) : _str(str) {
	}
	String::~String() {
	}
	string & String::str() {
		return _str;
	}
	UnsafeAutoRef<Object> String::call(const string & cmd, vector< UnsafeAutoRef<Object> > & args) {
		if (cmd == "length") {
			return UnsafeAutoRef<Object>(new Integer((long long)_str.size()));
		}
		return Object::call(cmd, args);
	}
	const char String::operator[] (const size_t & idx) const {
		return _str[idx];
	}
	string String::toString() const {
		return wrap_text(_str);
	}
	string String::toPrintString() const {
		return _str;
	}

	/**
	 * @brief pathname
	 */
	Pathname::Pathname() {
	}
	Pathname::Pathname(const File & file) : _file(file) {
	}
	Pathname::~Pathname() {
	}
	File & Pathname::file() {
		return _file;
	}
	string Pathname::basename_without_ext() {
		return _file.getFileNameWithoutExtension();
	}
	string Pathname::ext() {
		return _file.getExtension();
	}
	string Pathname::path() {
		return _file.getPath();
	}
	string Pathname::dirname() {
		return _file.getDirectory();
	}
	string Pathname::basename() {
		return _file.getFileName();
	}
	bool Pathname::exists() {
		return _file.exists();
	}
	bool Pathname::is_dir() {
		return _file.isDirectory();
	}
	bool Pathname::is_file() {
		try {
			return _file.isFile();
		} catch (Exception e) {
			throw NativeLispException(e);
		}
	}
	long long Pathname::size() {
		try {
			return _file.getSize();
		} catch (Exception e) {
			throw NativeLispException(e);
		}
	}
	osl_time_t Pathname::creation_time() {
		try {
			return _file.creationTime();
		} catch (Exception e) {
			throw NativeLispException(e);
		}
	}
	osl_time_t Pathname::last_modified_time(){
		try {
			return _file.lastModifiedTime();
		} catch (Exception e) {
			throw NativeLispException(e);
		}
	}
	string Pathname::toString() const {
		return "#p\"" + _file.getPath() + "\"";
	}
	string Pathname::toPrintString() const {
		return _file.getPath();
	}

	/**
	 * 
	 */
	FileDescriptor::FileDescriptor()
		: _fd(NULL), _autoclose(false) {
	}
	FileDescriptor::FileDescriptor(bool autoclose)
		: _fd(NULL), _autoclose(autoclose) {
	}
	FileDescriptor::FileDescriptor(FILE * _fd)
		: _fd(_fd), _autoclose(false) {
	}
	FileDescriptor::FileDescriptor(FILE * _fd, bool autoclose)
		: _fd(_fd), _autoclose(autoclose) {
	}
	FileDescriptor::~FileDescriptor() {
		if (_autoclose) {
			close();
		}
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
		string line;
		while (true) {
			int ch = fgetc(_fd);
            if (ch < 0 || ch == '\n') {
                break;
            }
			line.append(1, (char)ch);
		}
		return line;
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
	string FileDescriptor::toString() const {
		char str[256] = {0,};
		snprintf(str, sizeof(str), "#<FD:%p>", _fd);
		return string(str);
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

	UnsafeAutoRef<Scope> & Env::scope() {
		return _scope;
	}
	Heap<Var> & Env::heap() {
		return _heap;
	}
	_VAR Env::alloc(Var * var) {
		return heap().alloc(var);
	}
	void Env::gc() {
		size_t size = heap().size();
		unsigned long elapsed = heap().gc();
		if (_debug) {
			printf(" # GC / %d, dealloc: %d (%ld ms.) #\n",
				   (int)heap().size(), (int)(size - heap().size()), elapsed);
		}
	}
	void Env::clear() {
		_scope->clear();
		gc();
		heap().clear();
	}

	/**
	 * procedure
	 */

	Procedure::Procedure() {
	}
	Procedure::Procedure(const string & name) : _name(name) {
	}
	Procedure::~Procedure() {
	}
	string Procedure::toString() const {
		if (_name.empty()) {
			return "#<COMPILED FUNCTION/'NIL'>";
		}
		return "#<COMPILED FUNCTION/'" + _name  +"'>";
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
	UnsafeAutoRef<Scope> & Func::closure_scope() {
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
	_VAR Func::proc(Env & env, UnsafeAutoRef<Scope> & scope, _VAR & name, Sequence & args) {
		Parameters p_params = Parameters::read(env, scope, params());
		if (macro()) {
			p_params.bind(env, scope, closure_scope(), args, false);
			UnsafeAutoRef<Scope> local_scope(new Scope(*closure_scope()));
			local_scope->parent() = scope;
			return eval(env, scope, eval(env, local_scope, form()));
		}
		p_params.bind(env, scope, closure_scope(), args, true);
		UnsafeAutoRef<Scope> local_scope(new Scope(*closure_scope()));
		local_scope->parent() = scope;
		return eval(env, local_scope, form());
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
	Var::Var(const vector<_VAR> & lst) : _type(LIST), _obj(new Sequence(lst)) {
		_trace("init - list");
	}
	Var::Var(const Sequence & seq) : _type(LIST), _obj(new Sequence(seq)) {
		_trace("init - list");
	}
	Var::Var(bool bval) : _type(BOOLEAN), _obj(new Boolean(bval)) {
		if (!bval) {
			_type = NIL;
		}
		_trace("init - bool");
	}
	Var::Var(const Boolean & bval) : _type(BOOLEAN), _obj(new Boolean(bval)) {
		if (!bval.val()) {
			_type = NIL;
		}
		_trace("init - Boolean");
	}
	Var::Var(const Character & ch) : _type(CHARACTER), _obj(new Character(ch)) {
		_trace("init - character");
	}
	Var::Var(short inum) : _type(INTEGER), _obj(new Integer(inum)) {
		_trace("init - short");
	}
	Var::Var(int inum) : _type(INTEGER), _obj(new Integer(inum)) {
		_trace("init - int");
	}
	Var::Var(long inum) : _type(INTEGER), _obj(new Integer(inum)) {
		_trace("init - long");
	}
	Var::Var(long long inum) : _type(INTEGER), _obj(new Integer(inum)) {
		_trace("init - long long");
	}
	Var::Var(const Integer & inum) : _type(INTEGER), _obj(new Integer(inum)) {
		_trace("init - Integer");
	}
	Var::Var(float fnum) : _type(FLOAT), _obj(new Float(fnum)) {
		_trace("init - float");
	}
	Var::Var(double fnum) : _type(FLOAT), _obj(new Float(fnum)) {
		_trace("init - double");
	}
	Var::Var(const Float & fnum) : _type(FLOAT), _obj(new Float(fnum)) {
		_trace("init - Float");
	}
	Var::Var(Func * func) : _type(FUNC), _obj(func) {
		_trace("init - Func");
	}
	Var::Var(Procedure * procedure) : _type(NATIVE_PROC), _obj(procedure) {
		_trace("init - Procedure");
	}
	Var::Var(const File & file) : _type(PATHNAME), _obj(new Pathname(file)) {
		_trace("init - Pathname");
	}
	Var::Var(Pathname & pathname) : _type(PATHNAME), _obj(new Pathname(pathname)) {
		_trace("init - Pathname");
	}
	Var::Var(FILE * fd) : _type(FILE_DESCRIPTOR), _obj(new FileDescriptor(fd)) {
		_trace("init - FileDescriptor");
	}
	Var::Var(FILE * fd, bool autoclose) : _type(FILE_DESCRIPTOR), _obj(new FileDescriptor(fd, autoclose)) {
		_trace("init - FileDescriptor");
	}
	Var::Var(UnsafeAutoRef<Object> obj) : _type(OBJECT), _obj(obj) {
		_trace("init - Object");
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
			_obj = UnsafeAutoRef<Object>(new Keyword(token));
		} else if (token == "nil") {
			_type = NIL;
		} else if (token == "t") {
			_type = BOOLEAN;
		    _obj = UnsafeAutoRef<Object>(new Boolean(true));
		} else if (*token.begin() == '\"' && *token.rbegin() == '\"') {
			_type = STRING;
			_obj = UnsafeAutoRef<Object>(new String(unwrap_text(token)));
		} else if (Integer::isIntegerString(token)) {
			_type = INTEGER;
			_obj = UnsafeAutoRef<Object>(new Integer(Integer::toInteger(token)));
		} else if (Float::isFloatString(token)) {
			_type = FLOAT;
			_obj = UnsafeAutoRef<Object>(new Float(Float::toFloat(token)));
		} else if (*token.begin() == '#' && *(token.begin() + 1) == 'p') {
			_type = PATHNAME;
			string path = token.substr(3, token.length() - 4);
			_obj = UnsafeAutoRef<Object>(new Pathname(File(path)));
		} else {
			_type = SYMBOL;
			_obj = UnsafeAutoRef<Object>(new Symbol(token));;
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
		case NATIVE_PROC:
			return "NATIVE PROC";
		case PATHNAME:
			return "PATHNAME";
		case FILE_DESCRIPTOR:
			return "FILE DESCRIPTOR";
		case OBJECT:
			return "OBJECT:" + _obj->type_str();
		default:
			break;
		}
		throw LispException("[getTypeString()] unknown variable type / " + Text::toString(_type));
	}
	void Var::typeCheck(int t) const {
		if (_type != t) {
			throw LispException("Type check failed (required: " + getTypeString(t) +
								", but: " + getTypeString() + ") - " + toString());
		}
	}
	bool Var::isNil() const {return _type == NIL;}
	bool Var::isList() const {return _type == LIST;}
	bool Var::isSymbol() const {return _type == SYMBOL;}
	bool Var::isKeyword() const {return _type == KEYWORD;}
	bool Var::isBoolean() const {return _type == BOOLEAN;}
	bool Var::isNumber() const {return isInteger() || isFloat();}
	bool Var::isInteger() const {return _type == INTEGER;}
	bool Var::isFloat() const {return _type == FLOAT;}
	bool Var::isString() const {return _type == STRING;}
	bool Var::isCallable() const {
		return (isFunction() || isNativeProcedure());
	}
	bool Var::isFunction() const {return _type == FUNC;}
	bool Var::isNativeProcedure() const {return _type == NATIVE_PROC;}
	bool Var::isPathname() const {return _type == PATHNAME;}
	bool Var::isFileDescriptor() const {return _type == FILE_DESCRIPTOR;}

	const Symbol & Var::r_symbol() const {
		typeCheck(SYMBOL);
		return (const Symbol&)(*_obj);;
	}
	const Keyword & Var::r_keyword() const {
		typeCheck(KEYWORD);
		return (const Keyword&)(*_obj);;
	}
	const Character & Var::r_character() const {
		typeCheck(CHARACTER);
		return (const Character&)(*_obj);
	};
	const String & Var::r_string() const {
		typeCheck(STRING);
		return (const String&)(*_obj);
	}
	const Sequence & Var::r_list() const {
		typeCheck(LIST);
		return (const Sequence&)(*_obj);
	}
	const Boolean & Var::r_boolean() const {
		typeCheck(BOOLEAN);
		return (const Boolean&)(*_obj);
	}
	const Integer & Var::r_integer() const {
		typeCheck(INTEGER);
		return (const Integer&)(*_obj);
	}
	const Float & Var::r_float() const {
		typeCheck(FLOAT);
		return (const Float&)(*_obj);
	}
	const Pathname & Var::r_pathname() const {
		typeCheck(PATHNAME);
		return (const Pathname&)(*_obj);;
	}
	const Func & Var::r_func() const {
		typeCheck(FUNC);
		return (const Func&)(*_obj);
	}
	Symbol & Var::r_symbol() {
		typeCheck(SYMBOL);
		return (Symbol&)(*_obj);
	}
	Keyword & Var::r_keyword() {
		typeCheck(KEYWORD);
		return (Keyword&)(*_obj);
	}
	Character & Var::r_character() {
		typeCheck(CHARACTER);
		return (Character&)(*_obj);
	};
	String & Var::r_string() {
		typeCheck(STRING);
		return (String&)(*_obj);
	}
	Sequence & Var::r_list() {
		typeCheck(LIST);
		return (Sequence&)(*_obj);
	}
	Boolean & Var::r_boolean() {
		typeCheck(BOOLEAN);
		return (Boolean&)(*_obj);
	}
	Integer & Var::r_integer() {
		typeCheck(INTEGER);
		return (Integer&)(*_obj);
	}
	Float & Var::r_float() {
		typeCheck(FLOAT);
		return (Float&)(*_obj);
	}
	Pathname & Var::r_pathname() {
		typeCheck(PATHNAME);
		return (Pathname&)(*_obj);
	}
	Func & Var::r_func() {
		typeCheck(FUNC);
		return (Func&)(*_obj);
	}
	Procedure & Var::r_procedure() {
		typeCheck(NATIVE_PROC);
		return (Procedure&)(*_obj);
	}
	FileDescriptor & Var::r_fileDescriptor() {
		typeCheck(FILE_DESCRIPTOR);
		return (FileDescriptor&)(*_obj);
	}
	UnsafeAutoRef<Object> & Var::r_obj() {typeCheck(OBJECT); return _obj;}
	_VAR Var::expand(Env & env, UnsafeAutoRef<Scope> & scope, _VAR & name, Sequence & args) {
		if (!isFunction()) {
			throw LispException("Not Function / name: " + _SAFE_STRING(name) +
								" / type : '" + getTypeString() + "'");
		}
		if (r_func().macro() == false) {
			throw LispException("Not Macro");
		}
		Parameters params = Parameters::read(env, scope, r_func().params());
		params.bind(env, scope, r_func().closure_scope(), args, false);
		r_func().closure_scope()->parent() = scope;
		return eval(env, r_func().closure_scope(), r_func().form());
	}
	_VAR Var::proc(Env & env, UnsafeAutoRef<Scope> & scope, _VAR & name, Sequence & args) {
		if (!isCallable()) {
			throw LispException("not a callable / name: " + _SAFE_STRING(name) +
								" / type : '" + getTypeString() + "'");
		}
		if (isNativeProcedure()) {
			return r_procedure().proc(env, scope, name, args);
		}
		return r_func().proc(env, scope, name, args);
	}
	string Var::toString() const {
		switch (_type) {
		case NIL:
			return "NIL";
		case LIST:
		case SYMBOL:
		case KEYWORD:
		case STRING:
		case BOOLEAN:
		case CHARACTER:
		case INTEGER:
		case FLOAT:
		case FUNC:
		case NATIVE_PROC:
		case PATHNAME:
		case FILE_DESCRIPTOR:
		case OBJECT:
			return _obj->toString();
		default:
			break;
		}
		throw LispException("[toString()] unknown variable type / " + Text::toString(_type));
	}
	string Var::toPrintString() const {
		switch (_type) {
		case LIST:
		case SYMBOL:
		case KEYWORD:
		case STRING:
		case BOOLEAN:
		case CHARACTER:
		case INTEGER:
		case FLOAT:
		case FUNC:
		case NATIVE_PROC:
		case PATHNAME:
		case FILE_DESCRIPTOR:
		case OBJECT:
			return _obj->toPrintString();
		default:
			return toString();
		}
	}

	void Var::numberCheck() const {
		if (isNumber() == false) {
			throw LispException("not a number, but '" + getTypeString() + "'");
		}
	}

	void Var::numberOperationCheck(const Var & other) const {
		numberCheck();
		other.numberCheck();
	}

	Var & Var::operator+= (const Integer & inum) {
		r_integer() += inum;
		return *this;
	}
	Var & Var::operator-= (const Integer & inum) {
		r_integer() -= inum;
		return *this;
	}
	Var & Var::operator*= (const Integer & inum) {
		r_integer() *= inum;
		return *this;
	}
	Var & Var::operator/= (const Integer & inum) {
		r_integer() /= inum;
		return *this;
	}
	Var & Var::operator+= (const Float & fnum) {
		r_float() += fnum;
		return *this;
	}
	Var & Var::operator-= (const Float & fnum) {
		r_float() -= fnum;
		return *this;
	}
	Var & Var::operator*= (const Float & fnum) {
		r_float() *= fnum;
		return *this;
	}
	Var & Var::operator/= (const Float & fnum) {
		r_float() /= fnum;
		return *this;
	}

	bool Var::operator> (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) > (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator< (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) < (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator>= (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) >= (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator<= (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) <= (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator== (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) == (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator!= (const Var & other) const {
		numberOperationCheck(other);
		return (isInteger() ? r_integer() : r_float()) != (other.isInteger() ? other.r_integer() : other.r_float());
	}
	bool Var::operator> (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) > other;
	}
	bool Var::operator< (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) < other;
	}
	bool Var::operator>= (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) >= other;
	}
	bool Var::operator<= (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) <= other;
	}
	bool Var::operator== (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) == other;
	}
	bool Var::operator!= (const Integer & other) const {
		return (isInteger() ? r_integer() : r_float()) != other;
	}
	bool Var::operator> (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) > other;
	}
	bool Var::operator< (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) < other;
	}
	bool Var::operator>= (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) >= other;
	}
	bool Var::operator<= (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) <= other;
	}
	bool Var::operator== (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) == other;
	}
	bool Var::operator!= (const Float & other) const {
		return (isInteger() ? r_integer() : r_float()) != other;
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
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals,
						   const Parameter & rest)
		: _names(names), _optionals(optionals), _rest(rest) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals,
						   const map<Keyword, Parameter> & keywords)
		: _names(names), _optionals(optionals), _keywords(keywords) {
	}
	Parameters::Parameters(const vector<Parameter> & names, const vector<Parameter> & optionals,
						   const Parameter & rest, const map<Keyword, Parameter> & keywords)
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
	map<Keyword, Parameters::Parameter> & Parameters::keywords() {
		return _keywords;
	}

	inline static bool _is_special_keyword(const Symbol & token) {
		vector<string> lst = tokenize("&optional &rest &key");
		for (vector<string>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
			if (*iter == token) {
				return true;
			}
		}
		return false;
	}

	inline static vector<_VAR> s_read_tokens(Iterator<_VAR> & iter) {
		vector<_VAR> ret;
		while (iter.has()) {
			if ((*iter)->isSymbol() && _is_special_keyword((*iter)->r_symbol())) {
				break;
			}
			ret.push_back(*iter++);
		}
		return ret;
	}

	Parameters Parameters::read(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & tokens) {
		return Parameters::read(env, scope, tokens->r_list());
	}

	Parameters Parameters::read(Env & env, UnsafeAutoRef<Scope> & scope, Sequence & tokens) {
		Parameters params;
		Iterator<_VAR> iter = tokens.iter();
		
		vector<_VAR> names = s_read_tokens(iter);
		for (vector<_VAR>::iterator it = names.begin(); it != names.end(); it++) {
			params.names().push_back(Parameter(*it));
		}
		if (iter.has() && (*iter)->isSymbol() && (*iter)->r_symbol() == "&optional") {
			vector<_VAR> optionals = s_read_tokens(++iter);
			if (optionals.size() == 0) {
				throw LispException("&optional without any element");
			}
			for (vector<_VAR>::iterator it = optionals.begin(); it != optionals.end(); it++) {
				if ((*it)->isList() && (*it)->r_list().size() == 2) {
					params.optionals().push_back(Parameter((*it)->r_list()[0], (*it)->r_list()[1]));
				} else {
					params.optionals().push_back(Parameter(*it));
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
			for (vector<_VAR>::iterator it = keys.begin(); it != keys.end(); it++) {
				if ((*it)->isList() && (*it)->r_list().size() == 2) {
					_VAR v = (*it)->r_list()[0];
					_VAR i = (*it)->r_list()[1];
					Keyword k = Keyword::wrap(v->r_symbol());
					params.keywords()[k] = Parameter(v, i);
				} else {
					Keyword k = Keyword::wrap((*it)->r_symbol());
					params.keywords()[k] = Parameter(*it);
				}
			}
		}
		return params;
	}

	void Parameters::bind(Env & env, UnsafeAutoRef<Scope> & global_scope, UnsafeAutoRef<Scope> & lex_scope,
						  Sequence & tokens) {
		bind(env, global_scope, lex_scope, tokens, true);
	}
	void Parameters::bind(Env & env, UnsafeAutoRef<Scope> & global_scope, UnsafeAutoRef<Scope> & lex_scope,
						  Sequence & tokens, bool proc_eval) {
		
#define _PROC_VAR(E,S,T) (proc_eval ? eval(E,S,(T)) : (T))
		
		Iterator<_VAR> tokens_iter = tokens.iter();
		_CHECK_ARGS_MIN_COUNT(tokens, _names.size());
		for (vector<Parameter>::iterator iter = _names.begin(); iter != _names.end(); iter++) {
			_VAR v = _PROC_VAR(env, global_scope, *tokens_iter++);
			lex_scope->put_var(iter->name()->r_symbol(), v);
		}

		for (vector<Parameter>::iterator iter = _optionals.begin(); iter != _optionals.end(); iter++) {
			_VAR v = (tokens_iter.has() ? _PROC_VAR(env, global_scope, *tokens_iter++) :
					  (iter->initial().nil() ? _NIL(env) : _PROC_VAR(env, global_scope, iter->initial())));
			lex_scope->put_var(iter->name()->r_symbol(), v);
		}

		if (_rest.empty() == false) {
			lex_scope->put_var(_rest.name()->r_symbol(), _NIL(env));
		}

		for (map<Keyword, Parameter>::iterator iter = _keywords.begin(); iter != _keywords.end(); iter++) {
			_VAR v = (iter->second.initial().nil() ?
					  _NIL(env) : _PROC_VAR(env, global_scope, iter->second.initial()));
			lex_scope->put_var(iter->first.toSymbol(), v);
		}
		
		if (tokens_iter.has() == false) {
			return;
		}
		
		vector<_VAR> rest_ret;
		vector<_VAR> rest(tokens_iter.iter(), tokens.end());
		for (vector<_VAR>::iterator iter = rest.begin(); iter != rest.end(); iter++) {
			if ((*iter)->isKeyword()) {
				if (_keywords.find((*iter)->r_keyword()) == _keywords.end()) {
					throw LispException("Keyword '" + (*iter)->r_keyword().toString() + "' is not provided");
				}
				if (iter + 1 == rest.end()) {
					throw LispException("Unexpected end of tokens / keyword's value is missing");
				}
				Symbol sym = (*iter)->r_keyword().toSymbol();
				rest_ret.push_back(*iter);
				_VAR v = _PROC_VAR(env, global_scope, *(++iter));
				rest_ret.push_back(v);
				lex_scope->put_var(sym, v);
			} else {
				if (_rest.empty()) {
					throw LispException("&rest is not provided");
				}
				_VAR v = _PROC_VAR(env, global_scope, *iter);
				rest_ret.push_back(v);
			}
		}

		if (_rest.empty() == false) {
			lex_scope->put_var(_rest.name()->r_symbol(), _HEAP_ALLOC(env, rest_ret));
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
		for (map<Keyword, Parameter>::const_iterator iter = _keywords.begin(); iter != _keywords.end(); iter++) {
			if (iter != _keywords.begin()) {
				ret.append(", ");
			}
			ret.append(iter->second.toString());
		}
		ret.append("]");
		return ret;
	}

	/**
	 * @brief Server
	 */
	class LispServerSocket : public Object, public Closeable {
	private:
		ServerSocket _server;
	private:
		LispServerSocket(const LispServerSocket & other);
		LispServerSocket & operator= (const LispServerSocket & other);
	public:
		LispServerSocket(int port) : _server(port) {
			_server.setReuseAddr(true);
			_server.bind();
			_server.listen(50);
		}
		virtual ~LispServerSocket() {
			close();
		}
		ServerSocket & server() {
			return _server;
		}
		UnsafeAutoRef<Socket> accept() {
			return UnsafeAutoRef<Socket>(_server.accept());
		}
		virtual void close() {
			_server.close();
		}
		virtual string toString() const {
			return "<LispServerSocket>";
		}
	};

	/**
	 * @brief socket
	 */
	class LispSocket : public Object, public Closeable {
	private:
		UnsafeAutoRef<Socket> _socket;
	private:
        LispSocket(const LispSocket & other);
		LispSocket & operator= (const LispSocket & other);
	public:
		LispSocket(Socket * socket) : _socket(socket) {
		}
		LispSocket(UnsafeAutoRef<Socket> socket) : _socket(socket) {
		}
		virtual ~LispSocket() {
			close();
		}
		UnsafeAutoRef<Socket> & socket() {
			return _socket;
		}
		int recv(char * buf, size_t size) {
			return _socket->recv(buf, size);
		}
		int send(_VAR v) {
			switch (v->getType()) {
			case Var::STRING:
			{
				string str = v->toPrintString();
				return _socket->send(str.c_str(), str.size());
			}
			case Var::INTEGER:
			{
				char d = (char)_INT(v);
				return _socket->send(&d, 1);
			}
			default:
				throw LispException("Not supported variable type - " + v->getTypeString());
			}
		}
		virtual void close() {
			_socket->close();
		}
		virtual string toString() const {
			return "<LispSocket>";
		}
	};

	/**
	 *
	 */
	class LispDatabaseConnection : public Object, public Closeable {
	private:
		AutoRef<DatabaseConnection> _conn;
	private:
		LispDatabaseConnection(const LispDatabaseConnection & other);
		LispDatabaseConnection & operator= (const LispDatabaseConnection & other);
	public:
		LispDatabaseConnection(AutoRef<DatabaseConnection> conn)
			: _conn(conn) {
		}
		virtual ~LispDatabaseConnection() {
		}

		void connect(const string & host, int port, const string & username, const string & password, const string & dbname) {
			if (_conn->isConnected()) {
				throw LispException("Already connected");
			}
			_conn->connect(host, port, username, password, dbname);
		}

		void disconnect() {
			if (_conn->isConnected() == false) {
				throw LispException("Not connected yet");
			}
			_conn->disconnect();
		}

		virtual void close() {
			disconnect();
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
			return "<LispDatabaseConnection>";
		}			
	};

	/**
	 *
	 */
	class LispResultSet : public Object {
	private:
		AutoRef<ResultSet> _resultSet;
	public:
		LispResultSet(const AutoRef<ResultSet> & resultSet)
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
			return "<LispResultSet>";
		}
	};

	// built-in
	inline static void builtin_essential(Env & env);
	inline static void builtin_type(Env & env);
	inline static void builtin_algorithm(Env & env);
	inline static void builtin_list(Env & env);
	inline static void builtin_logic(Env & env);
	inline static void builtin_character(Env & env);
	inline static void builtin_string(Env & env);
	inline static void builtin_arithmetic(Env & env);
	inline static void builtin_mathematic(Env & env);
	inline static void builtin_io(Env & env);
	inline static void builtin_pathname(Env & env);
	inline static void builtin_file(Env & env);
	inline static void builtin_facility(Env & env);
	inline static void builtin_socket(Env & env);
	inline static void builtin_concurrent(Env & env);
	inline static void builtin_system(Env & env);
	inline static void builtin_date(Env & env);
	inline static void builtin_macro(Env & env);
	inline static void builtin_db(Env & env);
	inline static void builtin_benchmark(Env & env);
	
	inline static string format(Env & env, UnsafeAutoRef<Scope> & scope, const string & fmt, vector<_VAR> & args) {
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
				ret.append(eval(env, scope, iter.next())->toPrintString());
				s = f = (f + 2);
			} else if (fmt[f + 1] == 'd') {
				string num = eval(env, scope, iter.next())->toPrintString();
				ret.append(num);
				s = f = (f + 2);
			} else if (fmt[f + 1] == ':' && fmt[f + 2] == 'd') {
				string num = eval(env, scope, iter.next())->toPrintString();
				ret.append(Text::toCommaNumber(num));
				s = f = (f + 3);
			} else if (fmt[f + 1] == '$') {
				_VAR var = eval(env, scope, iter.next());
				string num;
				if (var->isInteger()) {
					num = var->toPrintString();
				} else if (var->isFloat()) {
					num = Text::format("%.2lf", _FLOAT(var));
				} else {
					throw LispException("wrong format '~$' - '" + var->toPrintString() + "'");
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

	_VAR pathname(Env & env, const _VAR & path) {
		if (path->isPathname()) {
			return path;
		}
		File file(path->toPrintString());
		return _HEAP_ALLOC(env, file);
	}

	string wrap_text(const string & txt) {
		return "\"" + txt + "\"";
	}
	
	string unwrap_text(const string & txt) {
		return txt.substr(1, txt.length() - 2);
	}

	template <typename T>
	inline static bool _contains(const vector<T> & _vec, const T & val) {
		typename vector<T>::const_iterator iter = _vec.begin();
		for (; iter != _vec.end(); iter++) {
			if (*iter == val) {
				return true;
			}
		}
		return false;
	}

	template <typename T, typename U>
	inline static bool _contains(const map<T, U> & _map, const T & val) {
		return (_map.find(val) != _map.end());
	}

	

	inline static bool _zero_p(_VAR v) {
		if (v->isInteger()) {
			return v->r_integer().zero_p();
		} else if (v->isFloat()) {
			return v->r_float().zero_p();
		}
		throw LispException("Not a number type - " + v->toString());
	}

	inline static _VAR _toFloat(Env & env, _VAR v) {
		if (v->isInteger()) {
			return _HEAP_ALLOC(env, (double)(_INT(v)));
		}
		return v;
	}

	inline static bool _eq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = _toFloat(env, v1);
			v2 = _toFloat(env, v2);
			return v1->r_float() == v2->r_float();
		}
		return v1->r_integer() == v2->r_integer();
	}

	inline static bool _gt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = _toFloat(env, v1);
			v2 = _toFloat(env, v2);
			return v1->r_float() > v2->r_float();
		}
		return v1->r_integer() > v2->r_integer();
	}

	inline static bool _lt(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = _toFloat(env, v1);
			v2 = _toFloat(env, v2);
			return v1->r_float() < v2->r_float();
		}
		return v1->r_integer() < v2->r_integer();
	}

	inline static bool _gteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = _toFloat(env, v1);
			v2 = _toFloat(env, v2);
			return v1->r_float() >= v2->r_float();
		}
		return v1->r_integer() >= v2->r_integer();
	}

	inline static bool _lteq(Env & env, _VAR v1, _VAR v2) {
		if (v1->isFloat() || v2->isFloat()) {
			v1 = _toFloat(env, v1);
			v2 = _toFloat(env, v2);
			return v1->r_float() <= v2->r_float();
		}
		return v1->r_integer() <= v2->r_integer();
	}

	inline static _VAR _plus(Env & env, _VAR v1, _VAR v2) {
		v1->numberCheck();
		v2->numberCheck();

		if (v1->isInteger() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_integer() + v2->r_integer());
		}
		if (v1->isInteger() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_integer() + v2->r_float());
		}
		if (v1->isFloat() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_float() + v2->r_integer());
		}
		if (v1->isFloat() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_float() + v2->r_float());
		}
        throw LispException("Weird: should not be reachable place");
	}

	inline static _VAR _minus(Env & env, _VAR v1, _VAR v2) {
		v1->numberCheck();
		v2->numberCheck();

		if (v1->isInteger() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_integer() - v2->r_integer());
		}
		if (v1->isInteger() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_integer() - v2->r_float());
		}
		if (v1->isFloat() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_float() - v2->r_integer());
		}
		if (v1->isFloat() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_float() - v2->r_float());
		}
        throw LispException("Weird: should not be reachable place");
	}

	inline static _VAR _multiply(Env & env, _VAR v1, _VAR v2) {
		v1->numberCheck();
		v2->numberCheck();

		if (v1->isInteger() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_integer() * v2->r_integer());
		}
		if (v1->isInteger() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_integer() * v2->r_float());
		}
		if (v1->isFloat() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_float() * v2->r_integer());
		}
		if (v1->isFloat() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_float() * v2->r_float());
		}
        throw LispException("Weird: should not be reachable place");
	}

	inline static _VAR _divide(Env & env, _VAR v1, _VAR v2) {
		v1->numberCheck();
		v2->numberCheck();
		if (_zero_p(v2)) {
			throw DivisionByZeroLispException("dvision by zero");
		}

		if (v1->isInteger() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_integer() / v2->r_integer());
		}
		if (v1->isInteger() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_integer() / v2->r_float());
		}
		if (v1->isFloat() && v2->isInteger()) {
			return _HEAP_ALLOC(env, v1->r_float() / v2->r_integer());
		}
		if (v1->isFloat() && v2->isFloat()) {
			return _HEAP_ALLOC(env, v1->r_float() / v2->r_float());
		}
        throw LispException("Weird: should not be reachable place");
	}

	inline static _VAR _cos(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _sin(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _tan(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _acos(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _asin(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _atan(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static _VAR _abs(Env & env, _VAR v) {
		if (v->isNumber() == false) {
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

	inline static size_t _length(_VAR lst) {
		return lst->r_list().size();
	}

	inline static _VAR _car(_VAR lst) {
		return lst->r_list()[0];
	}

	inline static Sequence _cdr(_VAR lst) {
		Sequence rest;
		if (_length(lst) > 1) {
			Iterator<_VAR> iter = lst->r_list().iter();
			for (iter++; iter.has(); iter++) {
				rest.push_back(*iter);
			}
		}
		return rest;
	}

	inline static _VAR _progn(Env & env, UnsafeAutoRef<Scope> & scope, const Sequence & forms,
					   size_t start_index) {
		_VAR ret;
		_FORI(forms, i, start_index) {
			ret = eval(env, scope, forms[i]);
		}
		return ret.nil() ? _NIL(env) : ret;
	}

	inline static _VAR _progn(Env & env, UnsafeAutoRef<Scope> & scope, const Sequence & forms) {
		return _progn(env, scope, forms, 0);
	}

	inline static _VAR _progn1(Env & env, UnsafeAutoRef<Scope> & scope, const Sequence & forms) {
		return _progn(env, scope, forms, 1);
	}

	/**
	 * get function
	 */
	inline static _VAR _function(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & var) {
		if (var->isFunction()) {
			return var;
		}
		if (var->isSymbol()) {
			return scope->rget_func(var->r_symbol());
		}
		_VAR func = eval(env, scope, var);
		if (func->isCallable()) {
			return func;
		}
		throw LispException("invalid function - " + var->toString());
	}

	inline static bool _isSpace(const char ch) {
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
				case 'p':
					tokens.push_back("#p");
					break;
				default:
					throw LispException("unexpected token '" + string(1, *iter) + "' after #");
				}
			} else if (!_isSpace(*iter)) {
				string tok;
				for (; iter != s.end() && !_isSpace(*iter) && *iter != '(' && *iter != ')'; iter++) {
					tok.append(1, *iter);
				}
				tokens.push_back(tok);
				iter--;
			}
		}
		return tokens;
	}

	inline static _VAR _read_from_tokens(Env & env, vector<string>::iterator & iter, vector<string>::iterator & end) {
		if (iter == end) {
			throw ParseLispException("syntax error - unexpected EOF");
		}
		if (*iter == "(") {
			vector<_VAR> lst;
			iter++;
			for (;*iter != ")"; iter++) {
				_VAR var = _read_from_tokens(env, iter, end);
				lst.push_back(var);
			}
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == ")") {
			throw ParseLispException("syntax error - unexpected ')'");
		} else if (*iter == "'") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "quote"));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "`") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "`"));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == ",") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, ","));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == ",@") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, ",@"));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "#'") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "function"));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "#\\") {
			vector<_VAR> lst;
			lst.push_back(_HEAP_ALLOC(env, "#\\"));
			lst.push_back(_read_from_tokens(env, ++iter, end));
			return _HEAP_ALLOC(env, lst);
		} else if (*iter == "#p") {
			vector<_VAR> lst;
			string path = "#p" + *(++iter);
			return _HEAP_ALLOC(env, path);
		} else {
			return _HEAP_ALLOC(env, *iter);
		}
	}

	_VAR parse(Env & env, const string & cmd) {
		vector<string> tokens = tokenize(BufferedCommandReader::eliminateComment(cmd));
		vector<string>::iterator iter = tokens.begin();
		vector<string>::iterator end = tokens.end();
		return _read_from_tokens(env, iter, end);
	}

	inline static bool _silentsymboleq(const _VAR & var, const string & sym) {
		return (var->isSymbol() && var->r_symbol() == sym);
	}

	inline static bool _silentkeywordeq(const _VAR & var, const string & key) {
		return (var->isKeyword() && var->r_keyword() == key);
	}

	inline static _VAR _quoty(Env & env, const _VAR & var) {
		vector<_VAR> qa;
		qa.push_back(_HEAP_ALLOC(env, "quote"));
		qa.push_back(var);
		return _HEAP_ALLOC(env, qa);
	}

	inline static _VAR _quote(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & var) {
		if (var->isList()) {
			Sequence lst = var->r_list(); // copy
			if (lst.empty()) {
				return _NIL(env);
			}
			if (lst[0]->isSymbol() && lst[0]->r_symbol() == ",") {
				throw EvalLispException("unexpected ','");
			}
			vector<_VAR> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				ret.push_back(_quote(env, scope, lst[i]));
			}
			return _HEAP_ALLOC(env, ret);
		}
		return var;
	}
	
	inline static _VAR _quasi(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & var) {
		if (var->isList()) {
			Sequence lst = var->r_list(); // copy
			if (lst.empty()) {
				return _NIL(env);
			}
			vector<_VAR> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				if (lst[i]->isList()) {
					Sequence & lv = lst[i]->r_list();
					if (lv.empty() == false && lv[0]->isSymbol()) {
						if (lv[0]->r_symbol() == ",") {
							_CHECK_ARGS_EXACT_COUNT(lv, 2);
							ret.push_back(eval(env, scope, lv[1]));
						} else if (lv[0]->r_symbol() == ",@") {
							_CHECK_ARGS_EXACT_COUNT(lv, 2);
							Sequence & llv = eval(env, scope, lv[1])->r_list();
							ret.insert(ret.end(), llv.begin(), llv.end());
						} else {
							ret.push_back(_quasi(env, scope, lst[i]));
						}
					}
				} else {
					ret.push_back(_quasi(env, scope, lst[i]));
				}
			}
			return _HEAP_ALLOC(env, ret);
		}
		return var;
	}

	_VAR eval(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & var) {
		if (var->isSymbol()) {
			return scope->rget_var(var->r_symbol());
		} else if (var->isList() == false) {
			return var;
		} else if (var->r_list().empty()) {
			return _NIL(env);
		} else {
			Sequence & lv = var->r_list();
			_VAR & cmd = lv[0];
			Sequence args(lv.begin() + 1, lv.end());
			if (_silentsymboleq(cmd, "quit")) {
				throw ExitLispException((args.size() > 0 ? (int)_INT(eval(env, scope, args[0])) : 0));
			} else {
				_VAR func = _function(env, scope, cmd);
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
		builtin_facility(env);
		builtin_socket(env);
		builtin_concurrent(env);
		builtin_system(env);
		builtin_date(env);
		builtin_macro(env);
		builtin_db(env);
		builtin_benchmark(env);
	}

	void builtin_essential(Env & env) {

		env.scope()->put_const(Symbol("!nil"), _HEAP_ALLOC(env, "nil"));
		env.scope()->put_const(Symbol("!t"), _HEAP_ALLOC(env, true));
		
		BEGIN_DECL_NATIVE(env, "eval");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return eval(env, scope, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "boundp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			if (scope->rsearch_var(eval(env, scope, args[0])->r_symbol()).nil()) {
				return _NIL(env);
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "fboundp");
		{
			// TODO: to global scope
			_CHECK_ARGS_MIN_COUNT(args, 1);
			if (scope->rsearch_func(eval(env, scope, args[0])->r_symbol()).nil()) {
				return _NIL(env);
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "lambda");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			args[0]->typeCheck(Var::LIST);
			vector<_VAR> form;
			form.push_back(_HEAP_ALLOC(env, "progn"));
			for (size_t i = 1; i < args.size(); i++) {
				form.push_back(args[i]);
			}
			Func * func = new Func(args[0], _HEAP_ALLOC(env, form));
			func->closure_scope()->registries() = scope->registries();
			return _HEAP_ALLOC(env, func);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "defun");
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
			return scope->rput_func(args[0]->r_symbol(),
									_HEAP_ALLOC(env, new Func(doc, lambda_list, _HEAP_ALLOC(env, form))));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "defparameter");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			scope->rput_var(args[0]->r_symbol(), eval(env, scope, args[1]));
			return args[0];
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "defvar");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_CHECK_ARGS_MAX_COUNT(args, 2);
			if (args.size() == 2 && scope->rsearch_var(args[0]->r_symbol()).nil() == true) {
				scope->rput_var(args[0]->r_symbol(), eval(env, scope, args[1]));
			}
			return args[0];
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "setf");
		{
			_CHECK_ARGS_EVEN_COUNT(args);
			_VAR ret;
			_FORI_STEP(args, i, 0, 2) {
				int type = args[i]->getType();
				_VAR a = eval(env, scope, args[i]);
				_VAR b = eval(env, scope, args[i + 1]);
				if (type != Var::SYMBOL && a->isNil()) {
					throw LispException("setf nil not allowed");
				}
				if (type != Var::SYMBOL && a->isList() && b->isList()) {
					for (size_t j = 0; j < a->r_list().size() && j < b->r_list().size(); j++) {
						(*a->r_list()[j]) = (*b->r_list()[j]);
					}
				} else {
					*a = *b;
				}
				ret = a;
			}
			return _NIL_OR_PASS(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "setq");
		{
			_CHECK_ARGS_EVEN_COUNT(args);
			_VAR ret;
			_FORI_STEP(args, i, 0, 2) {
				ret = scope->rput_var(args[i + 0]->r_symbol(), eval(env, scope, args[i + 1]));
			}
			return _NIL_OR_PASS(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "#\\");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, Character(args[0]->r_symbol().symbol()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "quote");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _quote(env, scope, args[0]);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "`");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _quasi(env, scope, args[0]);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "function");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _function(env, scope, args[0]);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "funcall");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR funcsym = eval(env, scope, args[0]);
			_VAR func = _function(env, scope, funcsym);
			Sequence fargs(args.begin() + 1, args.end());
			return func->proc(env, scope, name, fargs);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "let");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret;
			Sequence & decls = args[0]->r_list();
			UnsafeAutoRef<Scope> local_scope(new Scope(scope));
			for (vector<_VAR>::iterator iter = decls.begin(); iter != decls.end(); iter++) {
				Sequence decl = (*iter)->r_list(); // copy
				local_scope->put_var(decl[0]->r_symbol(), eval(env, scope, decl[1]));
			}
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				ret = eval(env, local_scope, *iter);
			}
			return _NIL_OR_PASS(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "if");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR val = eval(env, scope, args[0]);
			if (!val->isNil()) {
				return eval(env, scope, args[1]);
			} else if (args.size() > 2) {
				return eval(env, scope, args[2]);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "when");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			if (!test->isNil()) {
				return _progn1(env, scope, Sequence(args));
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "unless");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			if (test->isNil()) {
				return _progn1(env, scope, Sequence(args));
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "cond");
		{
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				Sequence lst = (*iter)->r_list(); // copy
				_CHECK_ARGS_MIN_COUNT(lst, 2);
				if (eval(env, scope, lst[0])->isNil() == false) {
					return _progn1(env, scope, lst);
				}
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "progn");
		{
			return _progn(env, scope, Sequence(args));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "while");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR pre_test = args[0];
			while (!eval(env, scope, pre_test)->isNil()) {
				eval(env, scope, args[1]);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "dolist");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			Sequence decl = args[0]->r_list(); // copy
			Symbol & param = decl[0]->r_symbol();
			Sequence lst = eval(env, scope, decl[1])->r_list(); // copy
			UnsafeAutoRef<Scope> local_scope(new Scope(scope));
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
				local_scope->put_var(param, *iter);
				eval(env, local_scope, args[1]);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "dotimes");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			Sequence steps = args[0]->r_list(); // copy
			Symbol & sym = steps[0]->r_symbol();
			long long limit = _INT(eval(env, scope, steps[1]));
			UnsafeAutoRef<Scope> local_scope(new Scope(scope));
			local_scope->put_var(sym, _HEAP_ALLOC(env, Integer(0)));
			for (; _INT(local_scope->get_var(sym)) < limit; (*local_scope->get_var(sym)) += Integer(1)) {
				eval(env, local_scope, args[1]);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "list");
		{
			vector<_VAR> elts;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				elts.push_back(eval(env, scope, *iter));
			}
			return _HEAP_ALLOC(env, elts);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "cons");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			vector<_VAR> ret;
			_VAR cons = eval(env, scope, args[0]);
			_VAR cell = eval(env, scope, args[1]);
			ret.push_back(cons);
			if (cell->isList()) {
				Sequence lst = cell->r_list(); // copy
				ret.insert(ret.end(), lst.begin(), lst.end());
			} else {
				ret.push_back(cell);
			}
			return _HEAP_ALLOC(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "car");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR lst = eval(env, scope, args[0]);
			if (_length(lst)) {
				return _car(lst);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "cdr");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, _cdr(eval(env, scope, args[0])));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "nth");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			size_t idx = (size_t)_INT(eval(env, scope, args[0]));
			Sequence & lst = eval(env, scope, args[1])->r_list();
			if (idx < lst.size()) {
				return lst[idx];
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "nthcdr");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			size_t idx = (size_t)_INT(eval(env, scope, args[0]));
			Sequence & lst = eval(env, scope, args[1])->r_list();
			if (idx < lst.size()) {
				vector<_VAR> rest;
				for (vector<_VAR>::iterator iter = lst.begin() + idx; iter != lst.end(); iter++) {
					rest.push_back(*iter);
				}
				return _HEAP_ALLOC(env, rest);
			}
			return _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "subseq");
		{
			_CHECK_ARGS_MIN_COUNT(args, 3);
			Sequence & lst = eval(env, scope, args[0])->r_list();
			long long start = _INT(eval(env, scope, args[1]));
			long long end = _INT(eval(env, scope, args[2]));
			vector<_VAR> ret;
			for (size_t i = start; i < end && i < lst.size(); i++) {
				ret.push_back(lst[i]);
			}
			return _HEAP_ALLOC(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "unwind-protect");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR ret;
			try {
				ret = eval(env, scope, args[0]);
			} catch (LispException e) {
				eval(env, scope, args[1]);
				throw e;
			}
			eval(env, scope, args[1]);
			return _NIL_OR_PASS(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "catch");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			try {
				_VAR ret;
				_FORI(args, i, 1) {
					ret = eval(env, scope, args[i]);
				}
				return _NIL_OR_PASS(env, ret);
			} catch (ThrowLispException e) {
				_VAR exp = eval(env, scope, args[0]);
				if (_EQ_NIL_OR_SYMBOL(e.except(), exp)) {
					return e.ret();
				}
				throw e;
			}
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "throw");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			throw ThrowLispException(eval(env, scope, args[0]), eval(env, scope, args[1]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "block");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			try {
				_VAR ret;
				_FORI(args, i, 1) {
					ret = eval(env, scope, args[i]);
				}
				return _NIL_OR_PASS(env, ret);
			} catch (ReturnLispException e) {
				if (_EQ_NIL_OR_SYMBOL(e.tag(), args[0])) {
					return e.var();
				}
				throw e;
			}
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "return-from");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret = _OPT_EVAL(env, scope, args, 1, _NIL(env));
			throw ReturnLispException(args[0], ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "defmacro");
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
			return scope->rput_func(args[0]->r_symbol(),
									_HEAP_ALLOC(env, new Func(true, doc, lambda_list, form)));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "macroexpand");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Sequence vars = eval(env, scope, args[0])->r_list(); // copy
			_CHECK_ARGS_MIN_COUNT(vars, 1);
			Sequence xargs(vars.begin() + 1, vars.end());
			return _function(env, scope, vars[0])->expand(env, scope, vars[0], xargs);
		}END_DECL_NATIVE;
	}

	void builtin_type(Env & env) {
		BEGIN_DECL_NATIVE(env, "symbolp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isSymbol());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "keywordp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isKeyword());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "listp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isList());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "booleanp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isBoolean());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "integerp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isInteger());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "floatp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFloat());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "stringp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isString());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "funcp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFunction());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "pathnamep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isPathname());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "streamp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isFileDescriptor());
		}END_DECL_NATIVE;
	}


	inline static void _bubble_sort(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & comp, Sequence & lst) {
		_VAR nil;
		Sequence fargs;
		fargs.push_back(_VAR());
		fargs.push_back(_VAR());
		for (size_t loop = 0; loop < lst.size() - 1; loop++) {
			int swap_count = 0;
			for (size_t i = 0; i < lst.size() - 1 - loop; i++) {
				fargs[0] = lst[i];
				fargs[1] = lst[i+1];
				bool test = comp->proc(env, scope, nil, fargs)->isNil(); // TODO: optimize
				if (test) {
					lst.swap(i, i+1);
					swap_count++;
				}
			}
			if (swap_count == 0) {
				break;
			}
		}
	}

	// int partition(int arr[], int l, int r) {
	// 	int pivot, i, j, t;
	// 	pivot = arr[l];
	// 	i = l; j = r+1;

	// 	while (1) {
	// 		do ++i; while (arr[i] <= pivot && i <= r);
	// 		do --j; while (arr[j] > pivot);
	// 		if (i >= j) break;
	// 		t = arr[i]; arr[i] = arr[j]; arr[j] = t;
	// 	}

	// 	t = arr[l]; arr[l] = arr[j]; arr[j] = t;
	// 	return j;
	// }

	// void quicksort(int arr[], int l, int r) {
	// 	int j;
	// 	if (l < r) {
	// 		j = partition(arr, l, r);
	// 		quicksort(arr, l, j-1);
	// 		quicksort(arr, j+1, r);
	// 	}
	// }

	inline static void _quicksort(Env & env, UnsafeAutoRef<Scope> & scope, const _VAR & comp,
						   Sequence & lst, int left, int right) {
		int i = left, j = right;
		_VAR pivot = lst[(left + right) / 2];

		_VAR nil;
		
		Sequence fargs;
		fargs.push_back(_VAR());
		fargs.push_back(_VAR());
 
		/* partition */
		do {
			while (true) {
				fargs[0] = lst[i];
				fargs[1] = pivot;
				bool test = (comp->proc(env, scope, nil, fargs)->isNil() == false); // arr[i] < pivot == false
				if (test == false) {
					break;
				}
				i++;
			}

			while (true) {
				fargs[0] = lst[j];
				fargs[1] = pivot;
				bool test = (comp->proc(env, scope, nil, fargs)->isNil() == false); // arr[i] > pivot == false
				if (test != false) {
					break;
				}
				j--;
			}
				
            if (i <= j) {
				lst.swap(i, j);
				i++;
				j--;
            }
		} while (i <= j);

		printf("i: %d, j: %d, left: %d, right: %d\n", i, j, left, right);
 
		/* recursion */
		if (left < j) {
            _quicksort(env, scope, comp, lst, left, j);
		}
		
		if (i < right) {
            _quicksort(env, scope, comp, lst, i, right);
		}
	}

	void builtin_algorithm(Env & env) {
		// TODO: refer - [http://www.lispworks.com/documentation/lw60/CLHS/Body/f_map.htm]
		BEGIN_DECL_NATIVE(env, "map");
		{
			_CHECK_ARGS_MIN_COUNT(args, 3);
			// TODO: check - http://clhs.lisp.se/Body/f_map.htm
			_VAR result_type = eval(env, scope, args[0]); /* TODO: use it */
			_VAR func = _function(env, scope, eval(env, scope, args[1]));
			Sequence ret;
			vector<_VAR> lists;
			size_t size = 0;
			for (size_t i = 2; i < args.size(); i++) {
				_VAR lst = eval(env, scope, args[i]);
				if (lst->r_list().size() > size) {
					size = lst->r_list().size();
				}
				lists.push_back(lst);
			}
			for (size_t i = 0; i < size; i++) {
				Sequence fargs;
				for (vector<_VAR>::iterator iter = lists.begin(); iter != lists.end(); iter++) {
					_VAR & lst = (*iter);
					fargs.push_back(_quoty(env, (i < lst->r_list().size() ? lst->r_list()[i] : _NIL(env))));
				}
				ret.push_back(func->proc(env, scope, name, fargs));
			}
			return _HEAP_ALLOC(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "mapcar");
		{
			// TODO: check - http://clhs.lisp.se/Body/f_map.htm
			_VAR func = _function(env, scope, eval(env, scope, args[0]));
			Sequence ret;
			vector<_VAR> lists;
			size_t size = 0;
			for (size_t i = 1; i < args.size(); i++) {
				_VAR lst = eval(env, scope, args[i]);
				lst->typeCheck(Var::LIST);
				if (lst->r_list().size() > size) {
					size = lst->r_list().size();
				}
				lists.push_back(lst);
			}
			for (size_t i = 0; i < size; i++) {
				Sequence fargs;
				for (vector<_VAR>::iterator iter = lists.begin(); iter != lists.end(); iter++) {
					_VAR & lst = (*iter);
					fargs.push_back(_quoty(env, (i < lst->r_list().size() ? lst->r_list()[i] : _NIL(env))));
				}
				ret.push_back(func->proc(env, scope, name, fargs));
			}
			return _HEAP_ALLOC(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "sort");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			Sequence lst = eval(env, scope, args[0])->r_list(); // copy
			_VAR comp = eval(env, scope, args[1]);
			comp = _function(env, scope, comp);

			if (lst.size() <= 1) {
				return _HEAP_ALLOC(env, lst);
			}

			// http://www.cplusplus.com/reference/algorithm/sort/

			// bubble sort
			_bubble_sort(env, scope, comp, lst);

			// quick sort
			// _quicksort(env, scope, comp, lst, 0, lst.size() - 1);
			
			return _HEAP_ALLOC(env, lst);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "reduce");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = _function(env, scope, eval(env, scope, args[0]));
			Sequence lst = eval(env, scope, args[1])->r_list(); // copy
			_VAR sum = (lst.size() > 0 ? lst[0] : _NIL(env));
			_VAR nil;
			for (size_t i = 1; i < lst.size(); i++) {
				Sequence fargs;
				fargs.push_back(sum);
				fargs.push_back(lst[i]);
				sum = func->proc(env, scope, nil, fargs);
			}
			return sum;
		}END_DECL_NATIVE;
	}

	void builtin_list(Env & env) {
		BEGIN_DECL_NATIVE(env, "length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Sequence lst = eval(env, scope, args[0])->r_list(); // copy
			return _HEAP_ALLOC(env, Integer((long long)lst.size()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			vector<_VAR> ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				Sequence lst = eval(env, scope, *iter)->r_list(); // copy
				ret.insert(ret.end(), lst.begin(), lst.end());
			}
			return _HEAP_ALLOC(env, ret);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "remove");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR val = eval(env, scope, args[0]);
			Sequence lst = eval(env, scope, args[1])->r_list(); // copy
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				if (val->toPrintString() == (*iter)->toPrintString()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "remove-if");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = _function(env, scope, eval(env, scope, args[0]));
			Sequence lst = eval(env, scope, args[1])->r_list(); // copy
			_VAR nil;
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				Sequence fargs;
				fargs.push_back(*iter);
				if (!func->proc(env, scope, nil, fargs)->isNil()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "remove-if-not");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR func = _function(env, scope, eval(env, scope, args[0]));
			Sequence lst = eval(env, scope, args[1])->r_list(); // copy
			_VAR nil;
			for (vector<_VAR>::iterator iter = lst.begin(); iter != lst.end();) {
				Sequence fargs;
				fargs.push_back(*iter);
				if (func->proc(env, scope, nil, fargs)->isNil()) {
					iter = lst.erase(iter);
				} else {
					iter++;
				}
			}
			return _HEAP_ALLOC(env, lst);
		}END_DECL_NATIVE;

		// TODO: implement
		// [https://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node144.html]
		// remove-if-not
		// [http://www.jtra.cz/stuff/lisp/sclr/push.html]
		// push
		// pop
	}

	void builtin_logic(Env & env) {
		BEGIN_DECL_NATIVE(env, "not");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->isNil());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "or");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR var;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				var = eval(env, scope, *iter);
				if (!var->isNil()) {
					break;
				}
			}
			return _NIL_OR_PASS(env, var);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "and");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR var = _TRUE(env);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				var = eval(env, scope, *iter);
				if (var->isNil()) {
					break;
				}
			}
			return var;
		}END_DECL_NATIVE;
	}

	void builtin_character(Env & env) {
		BEGIN_DECL_NATIVE(env, "character");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "characterp");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "alpha-char-p");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "alpha-numeric-p");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "digit-char-p");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "graphic-char-p");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "standard-char-p");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "upcase");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().upcase());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "downcase");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().downcase());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "upper-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().upper_case_p());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "lower-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().lower_case_p());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "both-case-p");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().both_case_p());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-code");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_character().raw());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "code-char");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, Character((char)_INT(eval(env, scope, args[0]))));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-int");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, _CHAR(eval(env, scope, args[0])));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-code-limit");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "charname");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, wrap_text(eval(env, scope, args[0])->r_character().charname()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-equal");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-lessp");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char-greaterp");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch == eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char/=");
		{
            throw LispException("Not implemented");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch < eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char>");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch > eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch <= eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "char>=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Character ch = eval(env, scope, args[0])->r_character();
			_FORI(args, i, 1) {
				if ((ch >= eval(env, scope, args[i])->r_character()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
	}

	void builtin_string(Env & env) {
		BEGIN_DECL_NATIVE(env, "string");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			switch (v->getType()) {
			case Var::STRING:
				return v;
			case Var::SYMBOL:
				return _HEAP_ALLOC(env, wrap_text(v->r_symbol().toString()));
			case Var::CHARACTER:
				return _HEAP_ALLOC(env, wrap_text(string(1, (char)v->r_character().raw())));
			default:
				break;
			}
			throw LispException(v->toString() + " is not a string designator");
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toPrintString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if ((val == eval(env, scope, *iter)->toPrintString()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toPrintString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if ((val < eval(env, scope, *iter)->toPrintString()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string>");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toPrintString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if ((val > eval(env, scope, *iter)->toPrintString()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toPrintString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if ((val <= eval(env, scope, *iter)->toPrintString()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string>=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string val = eval(env, scope, args[0])->toPrintString();
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				if ((val >= eval(env, scope, *iter)->toPrintString()) == false) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string-prefix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toPrintString();
			string dst = eval(env, scope, args[1])->toPrintString();
			return _HEAP_ALLOC(env, Text::startsWith(str, dst));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string-suffix-p");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string str = eval(env, scope, args[0])->toPrintString();
			string dst = eval(env, scope, args[1])->toPrintString();
			return _HEAP_ALLOC(env, Text::endsWith(str, dst));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Integer len((long long)eval(env, scope, args[0])->toPrintString().length());
			return _HEAP_ALLOC(env, len);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "string-append");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			string ret;
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				ret.append(eval(env, scope, *iter)->toPrintString());
			}
			return _HEAP_ALLOC(env, wrap_text(ret));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "format");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			_VAR test = eval(env, scope, args[0]);
			vector<_VAR> fargs(args.begin() + 2, args.end());
			string str = format(env, scope, args[1]->toPrintString(), fargs);
			if (!test->isNil()) {
				fputs(str.c_str(), stdout);
				fputs("\n", stdout);
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, wrap_text(str));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "enough-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 2);
			string org = eval(env, scope, args[0])->toPrintString();
			string prefix = eval(env, scope, args[1])->toPrintString();
			if (Text::startsWith(org, prefix)) {
				return _HEAP_ALLOC(env, wrap_text(org.substr(prefix.length())));
			}
			return _HEAP_ALLOC(env, wrap_text(org));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "replace-all");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 3);
			string org = eval(env, scope, args[0])->toPrintString();
			string target = eval(env, scope, args[1])->toPrintString();
			string replace = eval(env, scope, args[2])->toPrintString();
			return _HEAP_ALLOC(env, wrap_text(Text::replaceAll(org, target, replace)));
		}END_DECL_NATIVE;
	}
	void builtin_arithmetic(Env & env) {
		BEGIN_DECL_NATIVE(env, "+");
		{
			_VAR v = _HEAP_ALLOC(env, Integer(0));
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = _plus(env, v, eval(env, scope, *iter));
			}
			return v;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "-");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);				
			if (args.size() == 1) {
				return _minus(env, _HEAP_ALLOC(env, Integer(0)), eval(env, scope, args[0]));
			}
			_VAR v = eval(env, scope, args[0]);
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				v = _minus(env, v, eval(env, scope, *iter));
			}
			return v;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "*");
		{
			_VAR v = _HEAP_ALLOC(env, 1);
			for (vector<_VAR>::iterator iter = args.begin(); iter != args.end(); iter++) {
				v = _multiply(env, v, eval(env, scope, *iter));
			}
			return v;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "/");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				v = _divide(env, v, eval(env, scope, *iter));
			}
			return v;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "%");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			long long sum = _INT(eval(env, scope, args[0]));
			for (vector<_VAR>::iterator iter = args.begin() + 1; iter != args.end(); iter++) {
				sum %= _INT(eval(env, scope, *iter));
			}
			return _HEAP_ALLOC(env, Integer(sum));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!_eq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, ">");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!_gt(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "<");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!_lt(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, ">=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!_gteq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "<=");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR v = eval(env, scope, args[0]);
			_FORI(args, i, 1) {
				if (!_lteq(env, v, eval(env, scope, args[i]))) {
					return _NIL(env);
				}
			}
			return _TRUE(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "oddp");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_integer().odd_p());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "evenp");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _HEAP_ALLOC(env, eval(env, scope, args[0])->r_integer().even_p());
		}END_DECL_NATIVE;
	}
	void builtin_mathematic(Env & env) {
		env.scope()->put_var(Symbol("pi"), _HEAP_ALLOC(env, 3.141592653589793));
		BEGIN_DECL_NATIVE(env, "cos");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _cos(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "sin");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _sin(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "tan");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _tan(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "acos");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _acos(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "asin");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _asin(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "atan");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _atan(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "abs");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			return _abs(env, eval(env, scope, args[0]));
		}END_DECL_NATIVE;
	}
	void builtin_io(Env & env) {

		env.scope()->put_var(Symbol("*standard-output*"), _HEAP_ALLOC(env, stdout));
		env.scope()->put_var(Symbol("*standard-input*"), _HEAP_ALLOC(env, stdin));

		BEGIN_DECL_NATIVE(env, "read");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR ret;
			FileDescriptor & fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd.eof()) {
				return _TRUE(env);
			}
			BufferedCommandReader reader;
			while (!fd.eof() && reader.read(string(1, (char)fd.read())) < 1) {}
                
			vector<string> commands = reader.getCommands();
			for (vector<string>::iterator iter = commands.begin(); iter != commands.end(); iter++) {
				ret = parse(env, *iter);
				env.gc();
			}
			return _NIL_OR_PASS(env, ret);

        }END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "read-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor & fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (fd.eof()) {
				throw LispException("End Of File exception");
			}
			string line = fd.readline();
			return _HEAP_ALLOC(env, wrap_text(line));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "read-from-string");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			string str = eval(env, scope, args[0])->r_string().toPrintString();
			return parse(env, str);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "print");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR output = scope->rget_var(Symbol("*standard-output*"));
			if (args.size() == 2) {
				output = eval(env, scope, args[1]);
			}
			FileDescriptor & fd = output->r_fileDescriptor();
			_VAR msg = eval(env, scope, args[0]);
			fd.write(msg->toString());
			fd.write("\n");
			return msg;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "princ");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR output = scope->rget_var(Symbol("*standard-output*"));
			if (args.size() == 2) {
				output = eval(env, scope, args[1]);
			}
			FileDescriptor & fd = output->r_fileDescriptor();
			_VAR msg = eval(env, scope, args[0]);
			fd.write(msg->toPrintString());
			fd.write("\n");
			return msg;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "write-string");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR output = scope->rget_var(Symbol("*standard-output*"));
			if (args.size() == 2) {
				output = eval(env, scope, args[1]);
			}
			FileDescriptor & fd = output->r_fileDescriptor();
			string msg = eval(env, scope, args[0])->r_string().str();
			fd.write(msg);
			return _HEAP_ALLOC(env, wrap_text(msg));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "write-line");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR output = scope->rget_var(Symbol("*standard-output*"));
			if (args.size() == 2) {
				output = eval(env, scope, args[1]);
			}
			FileDescriptor & fd = output->r_fileDescriptor();
			string msg = eval(env, scope, args[0])->r_string().str();
			fd.write(msg);
			fd.write("\n");
			return _HEAP_ALLOC(env, wrap_text(msg));
		}END_DECL_NATIVE;
	}
	void builtin_pathname(Env & env) {
		BEGIN_DECL_NATIVE(env, "pathname");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_VAR path = pathname(env, eval(env, scope, args[0]));
			return path;
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "pathname-name");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, wrap_text(p.basename_without_ext()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "pathname-type");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, wrap_text(p.ext()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, wrap_text(p.path()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "directory-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, wrap_text(p.dirname()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "file-namestring");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, wrap_text(p.basename()));
		}END_DECL_NATIVE;
		
		// https://www.gnu.org/software/emacs/manual/html_node/elisp/Directory-Names.html
		
		BEGIN_DECL_NATIVE(env, "directory-file-name");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			string path = p.path();
			if (path.empty()) {
				return _NIL(env);
			}
			if (File::getSeparators().find(*path.rbegin()) != string::npos) {
				return _HEAP_ALLOC(env, wrap_text(path.substr(0, path.size() - 1)));
			}
			return _HEAP_ALLOC(env, wrap_text(path));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "file-name-directory");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			string path = p.path();
			size_t f = path.find_last_of(File::getSeparators());
			if (f == string::npos) {
				return _NIL(env);
			}
			return _HEAP_ALLOC(env, wrap_text(path.substr(0, f+1)));
		}END_DECL_NATIVE;
	}
	void builtin_file(Env & env) {
		BEGIN_DECL_NATIVE(env, "get-working-directory");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 0);
			return _HEAP_ALLOC(env, File(File::getCwd()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "list-directory");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			_VAR path = ((args.size() > 0) ? pathname(env, eval(env, scope, args[0])) : _HEAP_ALLOC(env, "#p\".\""));
			vector<File> files = File::list(path->r_pathname().path());
			vector<_VAR> lst;
			for (vector<File>::iterator iter = files.begin(); iter != files.end(); iter++) {
				lst.push_back(_HEAP_ALLOC(env, *iter));
			}
			return _HEAP_ALLOC(env, lst);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "probe-file");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return p.exists() ? _HEAP_ALLOC(env, p) : _NIL(env);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "dirp");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, p.is_dir());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "filep");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, p.is_file());
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "file-length");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, Integer((long long)p.size()));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "file-attribute-creation");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(p.creation_time()).sec));
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "file-attribute-lastmodified");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			return _HEAP_ALLOC(env, Integer((long long)osl_system_time_to_network_time(p.last_modified_time()).sec));
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "open");
		{
			Parameters params = Parameters::read(
				env, scope, parse(env, "(fname &key (if-does-not-exist :error) (if-exists :new-version))"));
			params.bind(env, scope, scope, args, true);
			Pathname & p = pathname(env, scope->get_var(Symbol("fname")))->r_pathname();
			const char * flags = "rb+";
			if (p.exists() == false) {
				// does not exists
				_VAR idne = scope->get_var(Symbol("if-does-not-exist"));
				if (idne->isNil()) {
					return _NIL(env);
				} else if (_silentkeywordeq(idne, ":error")) {
					throw LispException("cannot open " + p.toString());
				} else if (_silentkeywordeq(idne, ":create")) {
					flags = "wb+";
				}
			} else {
				// exists
				_VAR ie = scope->get_var(Symbol("if-exists"));
				if (ie->isNil()) {
					return _NIL(env);
				} else if (_silentkeywordeq(ie, ":append")) {
					flags = "ab+";
				} else if (_silentkeywordeq(ie, ":overwrite")) {
					flags = "wb+";
				}
			}
			return _HEAP_ALLOC(env, FileStream::s_open(p.path(), flags), true);
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "file-position");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			FileDescriptor & fd = eval(env, scope, args[0])->r_fileDescriptor();
			if (args.size() > 1) {
				fd.position((size_t)_INT(eval(env, scope, args[1])));
			}
			return _HEAP_ALLOC(env, Integer((long long)fd.position()));
				
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "close");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			eval(env, scope, args[0])->r_fileDescriptor().close();
			return _NIL(env);
		}END_DECL_NATIVE;
	}

	/**
	 * 
	 */
	class _cls_iter_base {
	public:
		_cls_iter_base() {}
		virtual ~_cls_iter_base() {}
		void _bind_vars(UnsafeAutoRef<Scope> & scope, const vector<Symbol> & syms, _VAR item) {
			if (syms.size() == 1) {
				scope->put_var(syms[0], item);
				return;
			}
			for (size_t i = 0; i < syms.size(); i++) {
				scope->put_var(syms[i], item->r_list()[i]);
			}
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) = 0;
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) = 0;
	};


	/**
	 * iter range
	 */
	class _cls_iter_range : public _cls_iter_base {
	private:
		bool _first;
		vector<Symbol> _vars;
		bool _downfrom;
		_VAR _curr;
		_VAR _from;
		_VAR _to;
		_VAR _under;
		_VAR _by;
	public:
		_cls_iter_range(Env & env, bool downfrom, vector<Symbol> vars, _VAR from, _VAR to, _VAR under, _VAR by)
			: _first(true), _downfrom(downfrom), _vars(vars), _from(from), _to(to), _under(under), _by(by) {
			if (_from.nil()) {
				_from = _HEAP_ALLOC(env, Integer(0));
			}
			_curr = _from;
			if (downfrom) {
				if (_by.nil()) {
					_by = _HEAP_ALLOC(env, Integer(-1));
				}
				if ((*_by) == Integer(0)) {
					throw LispException("'by' cannot be 0");
				}
				if ((*_by) > Integer(0)) {
					throw LispException("'by' cannot be greater than 0");
				}
			} else {
				if (_by.nil()) {
					_by = _HEAP_ALLOC(env, Integer(1));
				}
				if ((*_by) == Integer(0)) {
					throw LispException("'by' cannot be 0");
				}
				if ((*_by) < Integer(0)) {
					throw LispException("'by' cannot be less than 0");
				}
			}
		}
		virtual ~_cls_iter_range() {
		}
		bool & downfrom() {
			return _downfrom;
		}
		_VAR & curr() {
			return _curr;
		}
		_VAR & from() {
			return _from;
		}
		_VAR & to() {
			return _to;
		}
		_VAR & under() {
			return _under;
		}
		_VAR & by() {
			return _by;
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if (_first) {
				_first = false;
			} else {
				_curr = _plus(env, _curr, _by);
			}
			_bind_vars(scope, _vars, _curr);
			if (_downfrom) {
				if (_to.nil() == false) {
					if ((*_curr) < (*_to)) {
						return false;
					}
				}
				if (_under.nil() == false) {
					if ((*_curr) <= (*_under)) {
						return false;
					}
				}
			} else {
				if (_to.nil() == false) {
					if ((*_curr) > (*_to)) {
						return false;
					}
				}
				if (_under.nil() == false) {
					if ((*_curr) >= (*_under)) {
						return false;
					}
				}
			}
			return true;
		}
	};

	/**
	 * iter list
	 */
	class _cls_iter_list : public _cls_iter_base {
	private:
		size_t _idx;
		vector<Symbol> _syms;
		Sequence _seq;
	public:
		_cls_iter_list(const vector<Symbol> & syms, _VAR seq)
			: _idx(0), _syms(syms), _seq(seq->r_list()) {
		}
		virtual ~_cls_iter_list() {
		}
		Sequence & seq() {
			return _seq;
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if (_idx >= _seq.size()) {
				return false;
			}
			_bind_vars(scope, _syms, _seq[_idx++]);
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_while : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_while(_VAR stmt) : _stmt(stmt) {}
		virtual ~_cls_iter_while() {}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if (eval(env, scope, _stmt)->isNil() == false) {
				return false;
			}
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_unless : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_unless(_VAR stmt) : _stmt(stmt) {}
		virtual ~_cls_iter_unless() {}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if (eval(env, scope, _stmt)->isNil() == true) {
				return false;
			}
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_when : public _cls_iter_base {
	private:
		_VAR _test_stmt;
		_VAR _eval_stmt;
	public:
		_cls_iter_when(_VAR test_stmt, _VAR eval_stmt)
			: _test_stmt(test_stmt), _eval_stmt(eval_stmt) {}
		virtual ~_cls_iter_when() {}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if (eval(env, scope, _test_stmt)->isNil() == false && _eval_stmt.nil() == false) {
				eval(env, scope, _eval_stmt);
			}
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_assign : public _cls_iter_base {
	private:
		_VAR _stmt;
		vector<Symbol> _syms;
	public:
		_cls_iter_assign(const vector<Symbol> & syms, _VAR stmt) : _syms(syms), _stmt(stmt) {}
		virtual ~_cls_iter_assign() {}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			_bind_vars(scope, _syms, eval(env, scope, _stmt));
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_do : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_do(_VAR stmt) : _stmt(stmt) {}
		virtual ~_cls_iter_do() {}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			eval(env, scope, _stmt);
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_collect : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_collect(_VAR stmt) : _stmt(stmt) {
		}
		virtual ~_cls_iter_collect() {
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
			Symbol sym_ret("!ret");
			scope->put_var(sym_ret, _HEAP_ALLOC(env, Sequence()));
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			Symbol sym_ret("!ret");
			_VAR ret = scope->get_var(sym_ret);
			_VAR item = eval(env, scope, _stmt);
			ret->r_list().push_back(item);
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_thereis : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_thereis(_VAR stmt) : _stmt(stmt) {
		}
		virtual ~_cls_iter_thereis() {
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			_VAR v = eval(env, scope, _stmt);
			if(v->isNil() == false) {
				throw ReturnLispException(_NIL(env), v);
			}
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_always : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_always(_VAR stmt) : _stmt(stmt) {
		}
		virtual ~_cls_iter_always() {
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if(eval(env, scope, _stmt)->isNil() == false) {
				throw ReturnLispException(_NIL(env), _NIL(env));
			}
			return true;
		}
	};

	/**
	 * 
	 */
	class _cls_iter_never : public _cls_iter_base {
	private:
		_VAR _stmt;
	public:
		_cls_iter_never(_VAR stmt) : _stmt(stmt) {
		}
		virtual ~_cls_iter_never() {
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			if(eval(env, scope, _stmt)->isNil() == true) {
				throw ReturnLispException(_NIL(env), _NIL(env));
			}
			return true;
		}
	};

	/**
	 * iter
	 */
	class _cls_iter : public _cls_iter_base {
		Symbol _base_symbol;
		UnsafeAutoRef<_cls_iter_base> _iter;
	public:
		_cls_iter(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			read_statement(env, scope, stmt);
		}
		virtual ~_cls_iter() {
		}
		void read_statement(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			_VAR sym = _car(stmt);
			_base_symbol = sym->r_symbol();
			if (sym->r_symbol() == "for") {
				_read_for(env, scope, stmt);
			} else if (sym->r_symbol() == "while") {
				_read_while(env, scope, stmt);
			} else if (sym->r_symbol() == "unless") {
				_read_unless(env, scope, stmt);
			} else if (sym->r_symbol() == "when") {
				_read_when(env, scope, stmt);
			} else if (sym->r_symbol() == "do") {
				_read_do(env, scope, stmt);
			} else if (sym->r_symbol() == "collect") {
				_read_collect(env, scope, stmt);
			} else if (sym->r_symbol() == "thereis") {
				_read_thereis(env, scope, stmt);
			} else if (sym->r_symbol() == "always") {
				_read_always(env, scope, stmt);
			} else if (sym->r_symbol() == "never") {
				_read_never(env, scope, stmt);
			} else {
				throw LispException("Unexpected keyword - '" + sym->r_symbol().toString() +
									"' for loop facility");
			}
		}
		map<Symbol, _VAR> _read_map(_VAR stmt) {
			map<Symbol, _VAR> _map;
			Iterator<_VAR> iter = stmt->r_list().iter();
			while (iter.has()) {
				_VAR & var = *iter++;
				_VAR & val = *iter++;
				_map[var->r_symbol()] = val;
			}
			return _map;
		}
		void _set_var(UnsafeAutoRef<Scope> & scope, _VAR var, _VAR val) {
			if (var->isList()) {
				Iterator<_VAR> iter = var->r_list().iter();
				for (; iter.has(); iter++) {
					if (scope->rget_var((*iter)->r_symbol()).nil() == false) {
						throw LispException("Duplicated variable declaration");
					}
					scope->put_var((*iter)->r_symbol(), val);
				}
			} else if (var->isSymbol()) {
				if (scope->rget_var(var->r_symbol()).nil() == false) {
					throw LispException("Duplicated variable declaration");
				}
				scope->put_var(var->r_symbol(), val);
			} else {
				throw LispException("Not allowed var type - " + var->getTypeString());
			}
		}
		string _join(const vector<Symbol> & vec, const string & glue) {
			string ret;
			for (size_t i = 0; i < vec.size(); i++) {
				if (i > 0) {
					ret += glue;
				}
				ret += vec[i].toString();
			}
			return ret;
		}
		void _test_allowed_keywords(const map<Symbol, _VAR> & _map, const vector<Symbol> & keywords) {
			for (map<Symbol, _VAR>::const_iterator iter = _map.begin(); iter != _map.end(); iter++) {
				const Symbol & sym = iter->first;
				if (_contains(keywords, sym) == false) {
					throw LispException("Not in allowed keywords - '" + sym.toString() +
										"', allowed list [" + _join(keywords, ", ") + "]");
				}
			}
		}
		vector<Symbol> _make_symbol_vector(const char * arg, ...) {
			vector<Symbol> vec;
			if (arg != NULL) {
				vec.push_back(Symbol(arg));
			}
			va_list args;
			va_start(args, arg);
			const char * str = NULL;
			while ((str = (const char*)va_arg(args, const char *)) != NULL) {
				vec.push_back(Symbol(str));
			}
			va_end(args);
			return vec;
		}
		vector<Symbol> _read_var_syms(_VAR vars) {
			vector<Symbol> syms;
			if (vars->isList()) {
				Iterator<_VAR> iter = vars->r_list().iter();
				for (; iter.has(); iter++) {
					syms.push_back((*iter)->r_symbol());
				}
			} else {
				syms.push_back(vars->r_symbol());
			}
			return syms;
		}
		void _read_for(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			vector<Symbol> _syms = _read_var_syms(_map[Symbol("for")]);
			if (_contains(_map, Symbol("="))) {
				_test_allowed_keywords(_map, _make_symbol_vector("for",
																 "=",
																 (const char *)NULL));
				_VAR test_stmt = _map[Symbol("=")];
				_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_assign(_syms, test_stmt));
			} else if (_contains(_map, Symbol("in"))) {
				_test_allowed_keywords(_map, _make_symbol_vector("for",
																 "in",
																 (const char *)NULL));
				_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_list(_syms,
																   eval(env, scope,
																		_map[Symbol("in")])));
			} else if (_contains(_map, Symbol("downfrom"))) {
				_test_allowed_keywords(_map, _make_symbol_vector("for",
																 "downfrom",
																 "to",
																 "above",
																 "by",
																 (const char *)NULL));
				_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_range(env,
																	true,
																	_syms,
																	_map[Symbol("downfrom")],
																	_map[Symbol("to")],
																	_map[Symbol("above")],
																	_map[Symbol("by")]));
			} else {
				_test_allowed_keywords(_map, _make_symbol_vector("for",
																 "from",
																 "to",
																 "below",
																 "by",
																 (const char *)NULL));
				_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_range(env,
																	false,
																	_syms,
																	_map[Symbol("downfrom")],
																	_map[Symbol("to")],
																	_map[Symbol("below")],
																	_map[Symbol("by")]));
			}
		}
		void _read_while(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("while", (const char *)NULL));
			_VAR _stmt = _map[Symbol("while")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_while(_stmt));
		}
		void _read_unless(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("unless", (const char *)NULL));
			_VAR _stmt = _map[Symbol("unless")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_unless(_stmt));
		}
		void _read_when(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			_VAR test_stmt = stmt->r_list()[1];
			_VAR eval_stmt;
			if (stmt->r_list().size() > 2) {
				eval_stmt = stmt->r_list()[2];
			}
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_when(test_stmt, eval_stmt));
		}
		void _read_do(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("do", (const char *)NULL));
			_VAR _stmt = _map[Symbol("do")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_do(_stmt));
		}
		void _read_collect(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("collect", (const char *)NULL));
			_VAR _stmt = _map[Symbol("collect")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_collect(_stmt));
		}
		void _read_thereis(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("thereis", (const char *)NULL));
			_VAR _stmt = _map[Symbol("thereis")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_thereis(_stmt));
		}
		void _read_always(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("always", (const char *)NULL));
			_VAR _stmt = _map[Symbol("always")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_always(_stmt));
		}
		void _read_never(Env & env, UnsafeAutoRef<Scope> & scope, _VAR stmt) {
			map<Symbol, _VAR> _map = _read_map(stmt);
			_test_allowed_keywords(_map, _make_symbol_vector("never", (const char *)NULL));
			_VAR _stmt = _map[Symbol("never")];
			_iter = UnsafeAutoRef<_cls_iter_base>(new _cls_iter_never(_stmt));
		}
		virtual void on_start(Env & env, UnsafeAutoRef<Scope> & scope) {
			_iter->on_start(env, scope);
		}
		virtual bool iterate(Env & env, UnsafeAutoRef<Scope> & scope) {
			return _iter->iterate(env, scope);
		}
		string toString() const {
			return _base_symbol.toString();
		}
	};

	/**
	 * loop
	 */
	class _cls_loop {
	private:
		vector<_cls_iter> _iters;
	public:
		_cls_loop(Env & env, UnsafeAutoRef<Scope> & scope, _VAR seq) {
			Iterator<_VAR> iter = seq->r_list().iter();
			while (iter.has()) {
				_iters.push_back(_cls_iter(env, scope, *iter++));
			}
		}
		_cls_loop(Env & env, UnsafeAutoRef<Scope> & scope, Sequence & seq) {
			Iterator<_VAR> iter = seq.iter();
			while (iter.has()) {
				_iters.push_back(_cls_iter(env, scope, *iter++));
			}
		}
		virtual ~_cls_loop() {
		}
		void start(Env & env, UnsafeAutoRef<Scope> & scope) {
			vector<_cls_iter>::iterator iter = _iters.begin();
			for (; iter != _iters.end(); iter++) {
				iter->on_start(env, scope);
			}
		}
		_VAR loop(Env & env, UnsafeAutoRef<Scope> & scope) {
			try {
				start(env, scope);
				while (1) {
					vector<_cls_iter>::iterator iter = _iters.begin();
					for (; iter != _iters.end(); iter++) {
						if ((*iter).iterate(env, scope) == false) {
							if (scope->search_var(Symbol("!ret")).nil()) {
								return _NIL(env);
							} else {
								return scope->get_var(Symbol("!ret"));
							}
						}
					}
				}
			} catch (ReturnLispException e) {
				if (e.tag()->isNil() == false) {
					throw e;
				}
				return e.var();
			}
			return _NIL(env);
		}

		string toString() const {
			string str;
			vector<_cls_iter>::const_iterator iter = _iters.begin();
			for (int i = 0; iter != _iters.end(); iter++, i++) {
				if (i > 0) {
					str += ", ";
				}
				str += iter->toString();
			}
			return str;
		}
	};
	
	void builtin_facility(Env & env) {
		// BEGIN_DECL_NATIVE(env, "loop");
		// {
		// 	// TODO: implement
		// 	// [http://www.ai.sri.com/pkarp/loop.html]
		// 	// code:
		// 	//  (loop for x in '(a b c d e) do (print x))
		// 	throw LispException("not implemeneted");
		// }END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "iterate");
		{
			// TODO: implement
			// * [https://common-lisp.net/project/iterate/doc/Don_0027t-Loop-Iterate.html]
			// code:
			//  (iterate (for x in '(a b c d e)) (do (print x)))
			//  
			// pseudo:
			// while (update_iter()) {
			// 	eval(env, scope, actions);
			// }
			// 
			// spec:
			// for
			//     for x in '(1 2 3 4 5) ; eval once
			//     for (x . y) in '((1 . 1) (2 . 4) (3 . 9))
			//     --
			//     for x from 1
			//     for x to 10
			//     for x below 9
			//     for x from 1 to 5 below 9
			//     for x from 0 below (length s) ; eval once
			//     --
			//     for y = (* x 10) ; eval each loop
			//     --
			// test ; eval each loop
			//     while:
			//       while (< y 100) ; eval each loop
			//     unless:
			//     when:
			//       when (oddp x) do (print x)
			//       when (oddp x) collect x
			//       when (> x 10) return x
			// do
			//     user action
			// shorthands ; not allow mix
			//     collect
			//     sum
			//     thereis
			//     never
			//     always

			UnsafeAutoRef<Scope> local_scope(new Scope(scope));
			_cls_loop loop(env, local_scope, args);
			return loop.loop(env, local_scope);
		}END_DECL_NATIVE;
	}
	void builtin_socket(Env & env) {
		BEGIN_DECL_NATIVE(env, "server-open");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			int port = (int)_INT(eval(env, scope, args[0]));
			return _HEAP_ALLOC(env, UnsafeAutoRef<Object>(new LispServerSocket(port)));
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "connect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			_VAR host = eval(env, scope, args[0]);
			_VAR port = eval(env, scope, args[1]);
			try {
				UnsafeAutoRef<Socket> socket(new Socket(InetAddress(host->toPrintString(), (int)_INT(port))));
				socket->connect();
				return _HEAP_ALLOC(env, UnsafeAutoRef<Object>(new LispSocket(socket)));
			} catch (Exception e) {
				throw LispException("socket connect exception - " + e.message());
			}
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "accept");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			_VAR serv = eval(env, scope, args[0]);
			UnsafeAutoRef<Object> obj = serv->r_obj();
			LispServerSocket * server = ((LispServerSocket*)&obj);
			try {
				return _HEAP_ALLOC(env, UnsafeAutoRef<Object>(new LispSocket(server->accept())));
			} catch (Exception e) {
				throw LispException("socket accept exception - " + e.message());
			}
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "recv");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			_CHECK_ARGS_MAX_COUNT(args, 2);
			_VAR err;
			_VAR sock = eval(env, scope, args[0]);
			if (args.size() == 2) {
				err = eval(env, scope, args[1]);
			}
			UnsafeAutoRef<Object> obj = sock->r_obj();
			LispSocket * socket = ((LispSocket*)&obj);
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
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "send");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			_VAR sock = eval(env, scope, args[0]);
			_VAR data = eval(env, scope, args[1]);
			UnsafeAutoRef<Object> obj = sock->r_obj();
			LispSocket * socket = ((LispSocket*)&obj);
			int cnt = 0;
			try {
				if (data->isList()) {
					Sequence lst = data->r_list(); // copy
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
		}END_DECL_NATIVE;
	}
	void builtin_concurrent(Env & env) {
		// todo: fill it
	}
	void builtin_system(Env & env) {
		BEGIN_DECL_NATIVE(env, "system-type");
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
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "system");
		{
#if defined(USE_APPLE_STD)
            throw LispException("system() deprecated");
#else
            _CHECK_ARGS_MIN_COUNT(args, 1);
			Integer ret(system(eval(env, scope, args[0])->toPrintString().c_str()));
            return _HEAP_ALLOC(env, ret);
#endif
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "run-process");
		{
            _CHECK_ARGS_MIN_COUNT(args, 1);
			Process p(eval(env, scope, args[0])->toPrintString());
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
		}END_DECL_NATIVE;		
		BEGIN_DECL_NATIVE(env, "load");
		{
			_CHECK_ARGS_MIN_COUNT(args, 1);
			Pathname & p = pathname(env, eval(env, scope, args[0]))->r_pathname();
			FileStream stream(p.file(), "rb");
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
			return _TRUE(env);
		}END_DECL_NATIVE;
	}
	
	void builtin_date(Env & env) {
		env.scope()->put_var(Symbol("internal-time-units-per-second"), _HEAP_ALLOC(env, 1000));
		BEGIN_DECL_NATIVE(env, "now");
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
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "get-universal-time");
		{
			_CHECK_ARGS_MIN_COUNT(args, 0);
			return _HEAP_ALLOC(env, (long long)osl_get_time_network().sec);
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "decode-universal-time");
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
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "encode-universal-time");
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
		}END_DECL_NATIVE;
		BEGIN_DECL_NATIVE(env, "format-time-string-rfc8601");
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
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "format-time-string-rfc1123");
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
		}END_DECL_NATIVE;
	}

	void builtin_macro(Env & env) {
		compile(env, "(defmacro incf (x &optional (i 1)) `(setf ,x (+ ,x ,i)))");
		compile(env, "(defmacro decf (x &optional (i 1)) `(setf ,x (- ,x ,i)))");
		compile(env, "(defmacro 1+ (x) `(+ ,x 1))");
		compile(env, "(defmacro 1- (x) `(- ,x 1))");
		compile(env, "(defmacro cadr (x) `(car (cdr ,x)))");
	}

	void builtin_db(Env & env) {
		BEGIN_DECL_NATIVE(env, "db:load");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			string path = eval(env, scope, args[0])->toPrintString();
			string name = eval(env, scope, args[1])->toPrintString();
			DatabaseDriver::instance().load(name, AutoRef<Library>(new Library(path, name)));
			return _HEAP_ALLOC(env, name);
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:connect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 6);
			string name = eval(env, scope, args[0])->toPrintString();
			string hostname = eval(env, scope, args[1])->toPrintString();
			int port = (int)_INT(eval(env, scope, args[2]));
			string username = eval(env, scope, args[3])->toPrintString();
			string password = eval(env, scope, args[4])->toPrintString();
			string dbname = eval(env, scope, args[5])->toPrintString();
			LispDatabaseConnection * conn = new LispDatabaseConnection(DatabaseDriver::instance().getConnection(name));
			conn->connect(hostname, port, username, password, dbname);
			return _HEAP_ALLOC(env, UnsafeAutoRef<Object>(conn));
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:disconnect");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_obj());
			conn->disconnect();
			return _NIL(env);
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:query");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_obj());
			string sql = eval(env, scope, args[1])->toPrintString();
			return _HEAP_ALLOC(env, UnsafeAutoRef<Object>(new LispResultSet(conn->query(sql))));
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:fetch");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			LispResultSet * resultSet = ((LispResultSet *)&eval(env, scope, args[0])->r_obj());
			if (resultSet->next() == false) {
				return _NIL(env);
			}
			vector<_VAR> row;
			for (int i = 0; i < resultSet->fieldCount(); i++) {
				row.push_back(_HEAP_ALLOC(env, wrap_text(resultSet->getString(i))));
			}
			return _HEAP_ALLOC(env, row);
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:update");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			LispDatabaseConnection * conn = ((LispDatabaseConnection*)&eval(env, scope, args[0])->r_obj());
			string sql = eval(env, scope, args[1])->toPrintString();
			return _HEAP_ALLOC(env, conn->queryUpdate(sql));
		}END_DECL_NATIVE;

		BEGIN_DECL_NATIVE(env, "db:escape");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 1);
			string sql = eval(env, scope, args[0])->toPrintString();
			return _HEAP_ALLOC(env, wrap_text(Text::replaceAll(sql, "'", "\\'")));
		}END_DECL_NATIVE;
	}

	void builtin_benchmark(Env & env) {
		BEGIN_DECL_NATIVE(env, "benchmark:time");
		{
			_CHECK_ARGS_EXACT_COUNT(args, 2);
			string name = eval(env, scope, args[0])->toPrintString();
			unsigned long tick = tick_milli();
			try {
				_VAR ret = eval(env, scope, args[1]);
				printf("[%s] BENCHMARK time: %ld ms.\n", name.c_str(), (tick_milli() - tick));
				return ret;
			} catch (LispException e) {
				printf("[%s] BENCHMARK time: %ld ms.\n", name.c_str(), (tick_milli() - tick));
				throw e;
			}
		}END_DECL_NATIVE;
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

	void repl(Env & env) {
		BufferedCommandReader reader;
		FileStream in(stdin);
		fputs("> ", stdout);
		while (1) {
			string line = in.readline() + "\n";
			if (reader.read(line) > 0) {
				vector<string> & commands = reader.getCommands();
				vector<string>::iterator iter = commands.begin();
				for (; iter != commands.end(); iter++) {
					fputs(compile(env, *iter)->toString().c_str(), stdout);
					fputs("\n", stdout);
					env.gc();
				}
				reader.clearCommands();
				return;
			}
		}
	}
}
