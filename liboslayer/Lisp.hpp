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

namespace LISP {

	/**/
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
		OS::GCRef<Var> _tag;
		OS::GCRef<Var> _var;
	public:
		explicit ReturnLispException(OS::GCRef<Var> tag, OS::GCRef<Var> var);
		virtual ~ReturnLispException() throw();
		OS::GCRef<Var> tag();
		OS::GCRef<Var> var();
	};

	/**
	 * 
	 */
	class ThrowLispException : public LispException {
	private:
		OS::GCRef<Var> _exc;
		OS::GCRef<Var> _ret;
	public:
		explicit ThrowLispException(OS::GCRef<Var> except, OS::GCRef<Var> ret);
		virtual ~ThrowLispException() throw();
		OS::GCRef<Var> except();
		OS::GCRef<Var> ret();
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
	typedef OS::GCRef<Var> (*fn_proc)(Env & env, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args);
	extern std::string wrap_text(const std::string & txt);
	extern std::string unwrap_text(const std::string & txt);

	/**
	 * @brief registry
	 */
	
	class Registry : public std::map< std::string, OS::GCRef<Var> > {
	private:
	public:
		Registry();
		virtual ~Registry();
		bool contains(const std::string & k);
	};

	/**
	 * @brief scope
	 */
	class Scope {
	private:
		OS::AutoRef<Scope> _parent;
		std::map<std::string, Registry> _registries;
	public:
		Scope();
		virtual ~Scope();
		OS::AutoRef<Scope> & parent();
		void clear();
		std::map<std::string, Registry> & registries();
		Registry & registry(const std::string id);
		OS::GCRef<Var> rsearch_sym(const std::string & name);
		OS::GCRef<Var> rget_sym(const std::string & name);
		OS::GCRef<Var> rput_sym(const std::string & name, const OS::GCRef<Var> & var);
		OS::GCRef<Var> rsearch_func(const std::string & name);
		OS::GCRef<Var> rget_func(const std::string & name);
		OS::GCRef<Var> rput_func(const std::string & name, const OS::GCRef<Var> & var);
		OS::GCRef<Var> rsearch(const std::string id, const std::string & name);
		OS::GCRef<Var> rget(const std::string id, const std::string & name);
		OS::GCRef<Var> rput(const std::string id, const std::string & name, const OS::GCRef<Var> & var);
		OS::GCRef<Var> get_sym(const std::string & name);
	    void put_sym(const std::string & name, const OS::GCRef<Var> & var);
		OS::GCRef<Var> get_func(const std::string & name);
	    void put_func(const std::string & name, const OS::GCRef<Var> & var);
		OS::GCRef<Var> get(const std::string id, const std::string & name);
	    void put(const std::string id, const std::string & name, const OS::GCRef<Var> & var);
		int depth();
		std::string toString() const;
	};

	/**
	 * 
	 */
	class Object {
	private:
	public:
		Object() {
		}
		virtual ~Object() {
		}
		virtual void * native_ptr() {
			return NULL;
		}
		virtual int type() {
			return 0;
		}
		virtual std::string description() {
			return "<top object>";
		}
		virtual std::string type_str() {
			return "BaseObject";
		}
		virtual OS::AutoRef<Object> call(const std::string & cmd, std::vector< OS::AutoRef<Object> > & args) {
			throw LispException("no operation implemented - '" + cmd + "'");
		}
		virtual std::string toString() const {
			return "<lisp::object>";
		}
	};

	/**
	 * @brief procedure (built-in function)
	 */
	class Procedure : public Object {
	private:
		std::string _name;
		OS::GCRef<Var> _doc;
	public:
		Procedure(const std::string & name);
		virtual ~Procedure();
		std::string & name();
		OS::GCRef<Var> & doc();
		virtual OS::GCRef<Var> proc(Env & env, OS::AutoRef<Scope> scope, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args) = 0;
	};

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
	 * 
	 */
	class Pathname : public Object {
	private:
		OS::File _file;
	public:
		Pathname();
		Pathname(const OS::File & file);
		virtual ~Pathname();
		OS::File & file();
		std::string basename_without_ext();
		std::string ext();
		std::string path();
		std::string dirname();
		std::string basename();
		bool exists();
		bool is_dir();
		bool is_file();
		long long size();
		OS::osl_time_t creation_time();
		OS::osl_time_t last_modified_time();
		virtual std::string toString() const;
	};

	/**
	 * closeable
	 */
	class Closeable {
	public:
		Closeable() {}
		virtual ~Closeable() {}
		virtual void close() = 0;
	};

	/**
	 * auto closeable
	 */
	template<typename T>
	class AutoCloseable {
	private:
		Closeable * _closeable;
		bool _autoclose;
	public:
		AutoCloseable(T * closeable)
			: _closeable((Closeable*)closeable), _autoclose(true) {}
		AutoCloseable(T * closeable, bool autoclose)
			: _closeable((Closeable*)closeable), _autoclose(autoclose) {}
		virtual ~AutoCloseable() {
			if (_autoclose) {
				_closeable->close();
			}
		}
	};

	/**
	 * @brief lisp file descriptor
	 */
	class FileDescriptor : public Object, public Closeable, public AutoCloseable<Closeable> {
	private:
		FILE * _fd;
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
	 * @brief lisp extension type
	 */
	class LispExtension : public Object {
	public:
		LispExtension() {/**/}
		virtual ~LispExtension() {/**/}
	};

	/**
	 * @brief env
	 */
	class Env {
	private:
		static bool _debug;
		OS::AutoRef<Scope> _scope;
		OS::SharedHeap<Var> _heap;
		std::string _last_command;
	public:
		Env();
		virtual ~Env();
		static void setDebug(bool debug);
		void _trace(const std::string & msg);
		OS::AutoRef<Scope> & scope();
		OS::SharedHeap<Var> & heap();
		OS::GCRef<Var> alloc(Var * var);
		void gc();
		void clear();
		std::string & last_command();
	};

	/**
	 * @brief func
	 */
	class Func : public Object {
	private:
		bool _macro;
		OS::AutoRef<Scope> _closure_scope;
		OS::GCRef<Var> _doc;
		OS::GCRef<Var> _params;
		OS::GCRef<Var> _form;
	public:
		Func();
		Func(const OS::GCRef<Var> & params, const OS::GCRef<Var> & form);
		Func(bool macro, const OS::GCRef<Var> & params, const OS::GCRef<Var> & form);
		Func(const OS::GCRef<Var> & doc, const OS::GCRef<Var> & params, const OS::GCRef<Var> & form);
		Func(bool macro, const OS::GCRef<Var> & doc, const OS::GCRef<Var> & params, const OS::GCRef<Var> & form);
		virtual ~Func();
		bool empty() const;
		bool & macro();
		bool macro() const;
		OS::AutoRef<Scope> & closure_scope();
		OS::GCRef<Var> & doc();
		OS::GCRef<Var> & params();
		OS::GCRef<Var> & form();
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
		const static int PATHNAME = 10;
		const static int FILE_DESCRIPTOR = 11;
		const static int OBJECT = 12;
		const static int EXTENSION = 100;
		
	private:
		static bool _debug;
		int _type;
		std::string _symbol;
		std::string _keyword;
		std::string _str;
		std::vector<OS::GCRef<Var> > _lst;
		Character _ch;
		Func _func;
		OS::AutoRef<Procedure> _procedure;
		OS::AutoRef<Object> _obj;
		OS::AutoRef<LispExtension> _ext;
		
	public:
		explicit Var();
		explicit Var(const char * token);
		explicit Var(const std::string & token);
		explicit Var(std::vector<OS::GCRef<Var> > lst);
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
		explicit Var(const Func & func);
		explicit Var(OS::AutoRef<Procedure> procedure);
		explicit Var(OS::File & file);
		explicit Var(Pathname & pathname);
		explicit Var(std::FILE * fd);
		explicit Var(std::FILE * fd, bool autoclose);
		explicit Var(OS::AutoRef<Object> obj);
		explicit Var(OS::AutoRef<LispExtension> ext);
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
		bool isFunction() const;
		bool isPathname() const;
		bool isFileDescriptor() const;
		bool isObject() const;
		bool isExtension() const;

		const std::string & r_symbol() const;
		const std::string & r_keyword() const;
		const Character & r_character() const;
		const std::string & r_string() const;
		const std::vector<OS::GCRef<Var> > & r_list() const;
		const Boolean & r_boolean() const;
		const Integer & r_integer() const;
		const Float & r_float() const;
		const Pathname & r_pathname() const;
		const Func & r_func() const;
		std::string & r_symbol();
		std::string & r_keyword();
		Character & r_character();
		std::string & r_string();
		std::vector<OS::GCRef<Var> > & r_list();
		Boolean & r_boolean();
		Integer & r_integer();
		Float & r_float();
		Pathname & r_pathname();
		Func & r_func();
		OS::AutoRef<Procedure> & r_procedure();
		FileDescriptor & r_fileDescriptor();
		OS::AutoRef<Object> & r_obj();
		OS::AutoRef<LispExtension> & r_ext();

		OS::GCRef<Var> expand(Env & env, OS::AutoRef<Scope> scope, OS::GCRef<Var> name, std::vector< OS::GCRef<Var> > & args);
		OS::GCRef<Var> proc(Env & env, OS::AutoRef<Scope> scope, std::vector<OS::GCRef<Var> > & args);
		OS::GCRef<Var> proc(Env & env, OS::AutoRef<Scope> scope, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args);

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
		
		std::string toString() const;
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
			OS::GCRef<Var> _name;
			OS::GCRef<Var> _initial;
		public:
			Parameter();
			Parameter(const OS::GCRef<Var> & name);
			Parameter(const OS::GCRef<Var> & name, const OS::GCRef<Var> & initial);
			virtual ~Parameter();
			bool empty() const;
			OS::GCRef<Var> & name();
			OS::GCRef<Var> & initial();
			std::string toString() const;
		};

	private:
		std::vector<Parameter> _names;
		std::vector<Parameter> _optionals;
		Parameter _rest;
		std::map<std::string, Parameter> _keywords;
	public:
		Parameters();
		Parameters(const std::vector<Parameter> & names);
		Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals);
		Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals, const Parameter & rest);
		Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals, const std::map<std::string, Parameter> & keywords);
		Parameters(const std::vector<Parameter> & names, const std::vector<Parameter> & optionals, const Parameter & rest, const std::map<std::string, Parameter> & keywords);
		virtual ~Parameters();
		std::vector<Parameter> & names();
		std::vector<Parameter> & optionals();
		Parameter & rest();
		std::map<std::string, Parameter> & keywords();
		static Parameters parse(Env & env, OS::AutoRef<Scope> scope, std::vector< OS::GCRef<Var> > & tokens);
		void bind(Env & env, OS::AutoRef<Scope> global_scope, OS::AutoRef<Scope> lex_scope, std::vector< OS::GCRef<Var> > & tokens);
		void bind(Env & env, OS::AutoRef<Scope> global_scope, OS::AutoRef<Scope> lex_scope, std::vector< OS::GCRef<Var> > & tokens, bool proc_eval);
		std::string toString() const;
	};

	/**
	 * @brief lisp extern
	 */
	extern OS::GCRef<Var> pathname(Env & env, OS::GCRef<Var> path);
	extern void native(Env & env);
	extern void repl(Env & env);
	extern std::vector<std::string> tokenize(const std::string & s);
	extern OS::GCRef<Var> parse(Env & env, const std::string & cmd);
	extern OS::GCRef<Var> eval(Env & env, OS::AutoRef<Scope> scope, const OS::GCRef<Var> & var);
	extern OS::GCRef<Var> compile(Env & env, const std::string & cmd);
}

#endif
