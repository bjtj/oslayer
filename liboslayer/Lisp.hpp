#ifndef __LISP_HPP__
#define __LISP_HPP__

#include <string>
#include <map>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "os.hpp"
#include "File.hpp"
#include "Date.hpp"
#include "AutoRef.hpp"
#include "Text.hpp"
#include "Heap.hpp"
#include "Iterator.hpp"

namespace lisp {

    /**/
    class Symbol;
    class Keyword;
    class Integer;
    class Float;
    class Env;
    class Func;
    class Var;

    /**/
    DECL_NAMED_EXCEPTION(LispException);
    DECL_EXCEPTION(ParseLispException, LispException);
    DECL_EXCEPTION(EvalLispException, LispException);
    DECL_EXCEPTION(DivisionByZeroLispException, LispException);

    /**
     * 
     */
    class NativeLispException : public LispException {
    private:
	Exception _e;
    public:
	explicit NativeLispException(Exception & e);
	virtual ~NativeLispException() throw();
	virtual std::string toString() const;
    };

    /**
     * 
     */
    class ExitLispException : public LispException {
    private:
	int _code;
    public:
	explicit ExitLispException();
	explicit ExitLispException(int code);
	virtual ~ExitLispException() throw();
	int & code();
    };

    /**
     * 
     */
    class ReturnLispException : public LispException {
    private:
        osl::GCRef<Var> _tag;
	osl::GCRef<Var> _var;
    public:
	explicit ReturnLispException(osl::GCRef<Var> tag, osl::GCRef<Var> var);
	virtual ~ReturnLispException() throw();
	osl::GCRef<Var> tag();
	osl::GCRef<Var> var();
    };

    /**
     * 
     */
    class ThrowLispException : public LispException {
    private:
	osl::GCRef<Var> _exc;
	osl::GCRef<Var> _ret;
    public:
	explicit ThrowLispException(osl::GCRef<Var> except, osl::GCRef<Var> ret);
	virtual ~ThrowLispException() throw();
	osl::GCRef<Var> except();
	osl::GCRef<Var> ret();
    };

    /**
     * 
     */
    class UnboundLispException : public LispException {
    private:
	std::string _name;
    public:
	explicit UnboundLispException(const std::string & name);
	virtual ~UnboundLispException() throw();
	virtual std::string toString() const;
	std::string & name();
    };

    /**/
    typedef osl::GCRef<Var> (*fn_proc)(Env & env, osl::GCRef<Var> name, std::vector< osl::GCRef<Var> > & args);
    extern std::string wrap_text(const std::string & txt);
    extern std::string unwrap_text(const std::string & txt);

    /**
     * @brief registry IDs
     */

    typedef enum _REG_ID {
	REG_VARIABLE, REG_CONST, REG_FUNCTION
    } REG_ID;

    /**
     * @brief registry
     */
	
    class Registry : public std::map< Symbol, osl::GCRef<Var> > {
    private:
    public:
	Registry();
	virtual ~Registry();
	bool contains(const Symbol & k);
    };

    /**
     * @brief scope
     */
    class Scope {
    private:
	osl::UnsafeAutoRef<Scope> _parent;
	std::map<REG_ID, Registry> _registries;
    public:
	Scope();
	Scope(osl::UnsafeAutoRef<Scope> parent);
	virtual ~Scope();
	osl::UnsafeAutoRef<Scope> & parent();
	void clear();
	std::map<REG_ID, Registry> & registries();
	Registry & registry(const REG_ID & id);
	// var
	osl::GCRef<Var> search_var(const Symbol & sym);
	osl::GCRef<Var> rsearch_var(const Symbol & sym);
	osl::GCRef<Var> rget_var(const Symbol & sym);
	osl::GCRef<Var> rput_var(const Symbol & sym, const osl::GCRef<Var> & var);
	// const
	osl::GCRef<Var> search_const(const Symbol & sym);
	osl::GCRef<Var> rsearch_const(const Symbol & sym);
	osl::GCRef<Var> rget_const(const Symbol & sym);
	osl::GCRef<Var> rput_const(const Symbol & sym, const osl::GCRef<Var> & var);
	// func
	osl::GCRef<Var> search_func(const Symbol & sym);
	osl::GCRef<Var> rsearch_func(const Symbol & sym);
	osl::GCRef<Var> rget_func(const Symbol & sym);
	osl::GCRef<Var> rget_func(Symbol & sym);
	osl::GCRef<Var> rput_func(const Symbol & sym, const osl::GCRef<Var> & var);
	// 
	osl::GCRef<Var> search(const REG_ID & id, const Symbol & sym);
	osl::GCRef<Var> rsearch(const REG_ID & id, const Symbol & sym);
	osl::GCRef<Var> rget(const REG_ID & id, const Symbol & sym);
	osl::GCRef<Var> rput(const REG_ID & id, const Symbol & sym, const osl::GCRef<Var> & var);
	// var
	osl::GCRef<Var> get_var(const Symbol & sym);
	void put_var(const Symbol & sym, const osl::GCRef<Var> & var);
	// const
	osl::GCRef<Var> get_const(const Symbol & sym);
	void put_const(const Symbol & sym, const osl::GCRef<Var> & var);
	// func
	osl::GCRef<Var> get_func(const Symbol & sym);
	void put_func(const Symbol & sym, const osl::GCRef<Var> & var);
	// 
	osl::GCRef<Var> get(const REG_ID & id, const Symbol & sym);
	void put(const REG_ID & id, const Symbol & sym, const osl::GCRef<Var> & var);
	int depth();
	std::string toString() const;
    };

    /**
     * @brief Object
     */
    class Object {
    private:
    public:
	Object() {
	}
	virtual ~Object() {
	}
	virtual std::string type_str() {
	    return "BaseObject";
	}
	virtual osl::UnsafeAutoRef<Object> call(const std::string & cmd, std::vector< osl::UnsafeAutoRef<Object> > & args) {
	    throw LispException("no operation implemented - '" + cmd + "'");
	}
	virtual std::string toString() const {
	    return "<lisp::object>";
	}
	virtual std::string toPrintString() const {
	    return toString();
	}
    };

    /**
     * @brief
     */
    class Sequence : public Object {
    private:
	std::vector< osl::GCRef<Var> > _lst;
    public:
	explicit Sequence();
	explicit Sequence(const std::vector< osl::GCRef<Var> > & lst);
	explicit Sequence(std::vector< osl::GCRef<Var> >::iterator begin,
			  std::vector< osl::GCRef<Var> >::iterator end);
	explicit Sequence(std::vector< osl::GCRef<Var> >::const_iterator begin,
			  std::vector< osl::GCRef<Var> >::const_iterator end);
	virtual ~Sequence();
	osl::Iterator< osl::GCRef<Var> > iter();
	std::vector< osl::GCRef<Var> > & vec();
	bool empty() const;
	std::vector< osl::GCRef<Var> >::iterator begin();
	std::vector< osl::GCRef<Var> >::iterator end();
	std::vector< osl::GCRef<Var> >::const_iterator begin() const;
	std::vector< osl::GCRef<Var> >::const_iterator end() const;
	size_t size() const;
	std::vector< osl::GCRef<Var> >::iterator erase(std::vector< osl::GCRef<Var> >::iterator iter);
	void push_back(const osl::GCRef<Var> & var);
	void testIndexValid(const size_t & idx) const;
	void swap(const size_t & from, const size_t & to);
	Sequence subseq(const size_t & start, const size_t & end) const;
	osl::GCRef<Var> & operator[] (const size_t & idx);
	const osl::GCRef<Var> & operator[] (const size_t & idx) const;
	virtual std::string toString() const;
    };

    /**
     * @brief symbol
     */
    class Symbol : public Object {
    private:
	std::string _symbol;
    public:
	Symbol();
	Symbol(const std::string & symbol);
	virtual ~Symbol();
	std::string & symbol();
	std::string symbol() const;
	bool operator== (const std::string & other) const;
	bool operator== (const Symbol & other) const;
	bool operator< (const Symbol & other) const;
	bool operator> (const Symbol & other) const;
	virtual std::string toString() const;
    };

    bool operator== (const std::string & a, const Symbol & b);

    /**
     * @brief keyword
     */
    class Keyword : public Symbol {
    private:
    public:
	Keyword();
	Keyword(const std::string & keyword);
	virtual ~Keyword();
	std::string & keyword();
	std::string keyword() const;
	std::string keyword_without_token() const;
	Symbol toSymbol() const;
	static Keyword wrap(const Symbol & sym);
	virtual std::string toString() const;
    };

    bool operator== (const std::string & a, const Keyword & b);

    /**
     * @brief boolean
     */
    class Boolean : public Object {
    private:
	bool _val;
    public:
	Boolean();
	Boolean(bool val);
	virtual ~Boolean();
	bool & val();
	bool val() const;
	bool & operator* ();
	Boolean & operator= (bool val);
	Boolean & operator= (const Boolean & other);
	bool operator== (const bool & other) const;
	bool operator== (const Boolean & other) const;
	bool operator!= (const bool & other) const;
	bool operator!= (const Boolean & other) const;
	virtual std::string toString() const;
    };

    /**
     * @brief character
     * @ref http://clhs.lisp.se/Body/c_charac.htm
     */
    class Character : public Object {
    private:
	int _ch;
	std::string _name;
    public:
	Character();
	Character(int ch);
	Character(const std::string & name);
	virtual ~Character();
	int & raw();
	int raw() const;
	std::string & name();
	size_t width() const;
	bool alpha_char_p() const;
	bool alpha_numeric_p() const;
	bool digit_char_p() const;
	bool graphic_char_p() const;
	bool standard_char_p() const;
	Character upcase() const;
	Character downcase() const;
	bool upper_case_p() const;
	bool lower_case_p() const;
	bool both_case_p() const;
	int char_code() const;
	int char_int() const;
	int char_code_limit() const;
	std::string charname() const;
	bool equal(const Character & ch) const;
	bool lessp(const Character & ch) const;
	bool greaterp(const Character & ch) const;
	bool operator== (const Character & ch) const;
	bool operator/= (const Character & ch) const;
	bool operator< (const Character & ch) const;
	bool operator> (const Character & ch) const;
	bool operator<= (const Character & ch) const;
	bool operator>= (const Character & ch) const;
	virtual std::string toPrintString() const;
	virtual std::string toString() const;
    };

    /**
     * @brief lisp number
     */
    class Number : public Object {
    public:
	Number();
	virtual ~Number();
    };

    /**
     * @brief lisp integer
     */
    class Integer : public Number {
    private:
        long long num;
    public:
        Integer();
	Integer(short num);
	Integer(int num);
	Integer(long num);
        Integer(long long num);
        virtual ~Integer();
	static bool isIntegerString(const std::string & istr);
	static long long toInteger(const std::string & istr);
	bool zero_p() const;
	bool odd_p() const;
	bool even_p() const;
	long long raw() const;
        long long & operator* ();
        long long getInteger() const;
        Integer & operator+= (const Integer & other);
        Integer & operator-= (const Integer & other);
        Integer & operator*= (const Integer & other);
        Integer & operator/= (const Integer & other);
	Integer & operator%= (const Integer & other);
        Integer operator+ (const Integer & other) const;
        Integer operator- (const Integer & other) const;
        Integer operator* (const Integer & other) const;
        Integer operator/ (const Integer & other) const;
	Integer operator% (const Integer & other) const;
	bool operator> (const Integer & other) const;
	bool operator< (const Integer & other) const;
	bool operator>= (const Integer & other) const;
	bool operator<= (const Integer & other) const;
        bool operator== (const Integer & other) const;
        bool operator!= (const Integer & other) const;
	Float operator+ (const Float & other) const;
        Float operator- (const Float & other) const;
        Float operator* (const Float & other) const;
        Float operator/ (const Float & other) const;
	bool operator> (const Float & other) const;
	bool operator< (const Float & other) const;
	bool operator>= (const Float & other) const;
	bool operator<= (const Float & other) const;
        bool operator== (const Float & other) const;
        bool operator!= (const Float & other) const;
	virtual std::string toString() const;
    };

    /**
     * @brief lisp float
     */
    class Float : public Number {
    private:
	double num;
    public:
	Float();
	Float(float num);
	Float(double num);
	Float(const Integer & inum);
	virtual ~Float();
	static bool isFloatString(const std::string & istr);
	static double toFloat(const std::string & istr);
	bool zero_p() const;
	double raw() const;
	double & operator* ();
        Float & operator+= (const Float & other);
        Float & operator-= (const Float & other);
        Float & operator*= (const Float & other);
        Float & operator/= (const Float & other);
        Float operator+ (const Float & other) const;
        Float operator- (const Float & other) const;
        Float operator* (const Float & other) const;
        Float operator/ (const Float & other) const;
	bool operator> (const Float & other) const;
	bool operator< (const Float & other) const;
	bool operator>= (const Float & other) const;
	bool operator<= (const Float & other) const;
        bool operator== (const Float & other) const;
        bool operator!= (const Float & other) const;
	Float operator+ (const Integer & other) const;
        Float operator- (const Integer & other) const;
        Float operator* (const Integer & other) const;
        Float operator/ (const Integer & other) const;
	bool operator> (const Integer & other) const;
	bool operator< (const Integer & other) const;
	bool operator>= (const Integer & other) const;
	bool operator<= (const Integer & other) const;
        bool operator== (const Integer & other) const;
        bool operator!= (const Integer & other) const;
	virtual std::string toString() const;
    };

    /**
     * @brief string
     */
    class String : public Object {
    private:
	std::string _str;
    public:
	String();
	String(const std::string & str);
	virtual ~String();
	std::string & str();
	virtual osl::UnsafeAutoRef<Object> call(const std::string & cmd, std::vector< osl::UnsafeAutoRef<Object> > & args);
	const char operator[] (const size_t & idx) const;
	virtual std::string toString() const;
	virtual std::string toPrintString() const;
    };

    /**
     * @brief pathname
     */
    class Pathname : public Object {
    private:
	osl::File _file;
    public:
	Pathname();
	Pathname(const osl::File & file);
	virtual ~Pathname();
	osl::File & file();
	std::string ext();
	std::string path();
	std::string dirname();
	std::string name();
	std::string basename();
	bool exists();
	bool is_dir();
	bool is_file();
	long long size();
	osl::osl_time_t creation_time();
	osl::osl_time_t last_modified_time();
	virtual std::string toString() const;
	virtual std::string toPrintString() const;
    };

    /**
     * @brief closeable
     */
    class Closeable {
    public:
	Closeable() {}
	virtual ~Closeable() {}
	virtual void close() = 0;
    };


    /**
     * @brief lisp file descriptor
     */
    class FileDescriptor : public Object, public Closeable {
    private:
	FILE * _fd;
	bool _autoclose;
    private:
	// do not allow copy
	FileDescriptor(const FileDescriptor & other);
	FileDescriptor & operator= (const FileDescriptor & other);
    public:
	FileDescriptor();
	FileDescriptor(bool autoclose);
	FileDescriptor(FILE * _fd);
	FileDescriptor(FILE * _fd, bool autoclose);
	virtual ~FileDescriptor();
	FILE * fd();
	void testFd();
	bool eof();
	int read();
	std::string readline();
	void write(const std::string & data);
	size_t position();
	void position(size_t seek);
	virtual void close();
	virtual std::string toString() const;
    };

    /**
     * @brief env
     */
    class Env {
    private:
	static bool _debug;
	osl::UnsafeAutoRef< Scope > _scope;
	osl::Heap<Var> _heap;
    public:
	Env();
	virtual ~Env();
	static void setDebug(bool debug);
	void _trace(const std::string & msg);
	osl::UnsafeAutoRef< Scope > & scope();
	osl::GCRef<Var> nil();
	osl::GCRef<Var> t();
	osl::Heap<Var> & heap();
	osl::GCRef<Var> alloc(Var * var);
	void gc();
	void clear();
    };


#define LISP_PROCEDURE_PROC(E,S,N,A)					\
    virtual osl::GCRef<lisp::Var> proc(lisp::Env & E,			\
				       osl::UnsafeAutoRef<lisp::Scope> & S, \
				       osl::GCRef<lisp::Var> & N,	\
				       lisp::Sequence & A)

    /**
     * @brief procedure (built-in function)
     */
    class Procedure : public Object {
    private:
	std::string _name;
    public:
	Procedure();
	Procedure(const std::string & name);
	virtual ~Procedure();
	LISP_PROCEDURE_PROC(env, scope, name, args) = 0;
	virtual std::string toString() const;
    };

    /**
     * @brief func
     */
    class Func : public Object {
    private:
	bool _macro;
	osl::UnsafeAutoRef<Scope> _closure_scope;
	osl::GCRef<Var> _doc;
	osl::GCRef<Var> _params;
	osl::GCRef<Var> _form;
    public:
	Func();
	Func(const osl::GCRef<Var> & params, const osl::GCRef<Var> & form);
	Func(bool macro, const osl::GCRef<Var> & params, const osl::GCRef<Var> & form);
	Func(const osl::GCRef<Var> & description, const osl::GCRef<Var> & params, const osl::GCRef<Var> & form);
	Func(bool macro, const osl::GCRef<Var> & description, const osl::GCRef<Var> & params, const osl::GCRef<Var> & form);
	virtual ~Func();
	bool empty() const;
	bool & macro();
	bool macro() const;
	osl::UnsafeAutoRef<Scope> & closure_scope();
	osl::GCRef<Var> & doc();
	osl::GCRef<Var> & params();
	osl::GCRef<Var> & form();
	virtual osl::GCRef<Var> proc(Env & env, osl::UnsafeAutoRef<Scope> & scope, osl::GCRef<Var> & name, Sequence & args);
	std::string toString() const;
    };

    /**
     * @brief Var
     */
    class Var {
    public:
	const static int NIL = 0;
	const static int SYMBOL = 1;
	const static int KEYWORD = 2;
	const static int LIST = 3;
	const static int BOOLEAN = 4;
	const static int CHARACTER = 5;
	const static int INTEGER = 6;
	const static int FLOAT = 7;
	const static int STRING = 8;
	const static int FUNC = 9;
	const static int NATIVE_PROC = 10;
	const static int PATHNAME = 11;
	const static int FILE_DESCRIPTOR = 12;
	const static int OBJECT = 13;
		
    private:
	static bool _debug;
	int _type;
	osl::UnsafeAutoRef<Object> _obj;
		
    public:
	explicit Var();
	explicit Var(const char * token);
	explicit Var(const std::string & token);
	explicit Var(const std::vector< osl::GCRef<Var> > & lst);
	explicit Var(const Sequence & lst);
	explicit Var(bool bval);
	explicit Var(const Boolean & bval);
	explicit Var(const Character & ch);
	explicit Var(short inum);
	explicit Var(int inum);
	explicit Var(long inum);
	explicit Var(long long inum);
	explicit Var(const Integer & inum);
	explicit Var(float dnum);
	explicit Var(double dnum);
	explicit Var(const Float & fnum);
	explicit Var(Func * func);
	explicit Var(Procedure * procedure);
	explicit Var(const osl::File & file);
	explicit Var(Pathname & pathname);
	explicit Var(std::FILE * fd);
	explicit Var(std::FILE * fd, bool autoclose);
	explicit Var(osl::UnsafeAutoRef<Object> obj);
	virtual ~Var();

	static void setDebug(bool debug);
	void _trace(const std::string & msg);

	void init(const std::string & token);
	int getType();
	std::string getTypeString() const;
	std::string getTypeString(int type) const;
	void typeCheck(int t) const;
	bool isNil() const;
	bool isList() const;
	bool isKeyword() const;
	bool isSymbol() const;
	bool isBoolean() const;
	bool isNumber() const;
	bool isInteger() const;
	bool isFloat() const;
	bool isString() const;
	bool isCallable() const;
	bool isFunction() const;
	bool isNativeProcedure() const;
	bool isPathname() const;
	bool isFileDescriptor() const;
	bool isObject() const;

	const Symbol & r_symbol() const;
	const Keyword & r_keyword() const;
	const Character & r_character() const;
	const String & r_string() const;
	const Sequence & r_list() const;
	const Boolean & r_boolean() const;
	const Integer & r_integer() const;
	const Float & r_float() const;
	const Pathname & r_pathname() const;
	const Func & r_func() const;
	Symbol & r_symbol();
	Keyword & r_keyword();
	Character & r_character();
	String & r_string();
	Sequence & r_list();
	Boolean & r_boolean();
	Integer & r_integer();
	Float & r_float();
	Pathname & r_pathname();
	Func & r_func();
	Procedure & r_procedure();
	FileDescriptor & r_fileDescriptor();
	osl::UnsafeAutoRef<Object> & r_obj();

	osl::GCRef<Var> expand(Env & env, osl::UnsafeAutoRef<Scope> & scope,
			       osl::GCRef<Var> & name, Sequence & args);
	osl::GCRef<Var> proc(Env & env, osl::UnsafeAutoRef<Scope> & scope,
			     osl::GCRef<Var> & name, Sequence & args);

	void numberCheck() const;
	void numberOperationCheck(const Var & other) const;
	Var & operator+= (const Integer & inum);
	Var & operator-= (const Integer & inum);
	Var & operator*= (const Integer & inum);
	Var & operator/= (const Integer & inum);
	Var & operator+= (const Float & fnum);
	Var & operator-= (const Float & fnum);
	Var & operator*= (const Float & fnum);
	Var & operator/= (const Float & fnum);
	bool operator> (const Var & other) const;
	bool operator< (const Var & other) const;
	bool operator>= (const Var & other) const;
	bool operator<= (const Var & other) const;
        bool operator== (const Var & other) const;
        bool operator!= (const Var & other) const;
	bool operator> (const Integer & other) const;
	bool operator< (const Integer & other) const;
	bool operator>= (const Integer & other) const;
	bool operator<= (const Integer & other) const;
        bool operator== (const Integer & other) const;
        bool operator!= (const Integer & other) const;
	bool operator> (const Float & other) const;
	bool operator< (const Float & other) const;
	bool operator>= (const Float & other) const;
	bool operator<= (const Float & other) const;
        bool operator== (const Float & other) const;
        bool operator!= (const Float & other) const;
		
	std::string toString() const;
	std::string toPrintString() const;
    };

    /**
     * @brief BufferedCommandReader
     */
    class BufferedCommandReader {
    private:
	std::vector<std::string> commands;
	std::string buffer;
    public:
	BufferedCommandReader();
	virtual ~BufferedCommandReader();

	void clearCommands();
	void clearBuffer();
	static std::string eliminateComment(const std::string & text);
	static size_t testComplete(const std::string & text);
	size_t read(const std::string & text);
	size_t size();
	std::vector<std::string> & getCommands();
	std::string & operator[] (size_t idx);
    };

    /**
     * 
     */
    class Parameters
    {
    public:
	
	/**
	 * 
	 */
	class Parameter
	{
	private:
	    osl::GCRef<Var> _name;
	    osl::GCRef<Var> _initial;
	public:
	    Parameter();
	    Parameter(const osl::GCRef<Var> & name);
	    Parameter(const osl::GCRef<Var> & name, const osl::GCRef<Var> & initial);
	    virtual ~Parameter();
	    bool empty() const;
	    osl::GCRef<Var> & name();
	    osl::GCRef<Var> & initial();
	    std::string toString() const;
	};

    private:
	std::vector<Parameter> _names;
	std::vector<Parameter> _optionals;
	Parameter _rest;
	std::map<Keyword, Parameter> _keywords;
    public:
	Parameters();
	Parameters(const std::vector<Parameter> & names);
	Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals);
	Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals,
		   const Parameter & rest);
	Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals,
		   const std::map<Keyword, Parameter> & keywords);
	Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals,
		   const Parameter & rest, const std::map<Keyword, Parameter> & keywords);
	virtual ~Parameters();
	std::vector<Parameter> & names();
	std::vector<Parameter> & optionals();
	Parameter & rest();
	std::map<Keyword, Parameter> & keywords();
	static Parameters read(Env & env, osl::UnsafeAutoRef<Scope> & scope, const osl::GCRef<Var> & tokens);
	static Parameters read(Env & env, osl::UnsafeAutoRef<Scope> & scope, Sequence & tokens);
	void bind(Env & env, osl::UnsafeAutoRef<Scope> & global_scope, osl::UnsafeAutoRef<Scope> & lex_scope,
		  Sequence & tokens);
	void bind(Env & env, osl::UnsafeAutoRef<Scope> & global_scope, osl::UnsafeAutoRef<Scope> & lex_scope,
		  Sequence & tokens, bool proc_eval);
	std::string toString() const;
    };

    /**
     * @brief lisp extern
     */
    extern osl::GCRef<Var> pathname(Env & env, const osl::GCRef<Var> & path);
    extern void native(Env & env);
    extern void repl(Env & env);
    extern std::vector<std::string> tokenize(const std::string & s);
    extern osl::GCRef<Var> parse(Env & env, const std::string & cmd);
    extern osl::GCRef<Var> eval(Env & env, osl::UnsafeAutoRef<Scope> & scope, const osl::GCRef<Var> & var);
    extern osl::GCRef<Var> compile(Env & env, const std::string & cmd);
}

#endif
