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
#include "Ref.hpp"
#include "Text.hpp"
#include "Heap.hpp"

namespace LISP {

	/**/
	class Env;
	class Func;
	class Var;

	/**/
	DECL_NAMED_EXCEPTION(LispException);
	DECL_EXCEPTION(ParseLispException, LispException);
	DECL_EXCEPTION(EvalLispException, LispException);

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
		OS::GCRef<Var> _except;
		OS::GCRef<Var> _ret;
	public:
		explicit ThrowLispException(OS::GCRef<Var> except, OS::GCRef<Var> ret);
		virtual ~ThrowLispException() throw();
		OS::GCRef<Var> except();
		OS::GCRef<Var> ret();
	};

	/**/
	typedef OS::GCRef<Var> (*fn_proc)(Env & env, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args);
	extern std::string wrap_text(const std::string & txt);
	extern std::string unwrap_text(const std::string & txt);

	/**
	 * @brief scope
	 */
	class Scope {
	private:
		UTIL::Ref<Scope> _parent;
		std::map<std::string, OS::GCRef<Var> > _registry;
	public:
		Scope();
		virtual ~Scope();
		UTIL::Ref<Scope> & parent();
		void clear();
		OS::GCRef<Var> rsearch(const std::string & name);
		OS::GCRef<Var> & rget(const std::string & name);
		std::map<std::string, OS::GCRef<Var> > & registry();
		OS::GCRef<Var> get(const std::string & name);
	    void put(const std::string & name, const OS::GCRef<Var> & var);
		int depth();
		std::string toString() const;
	};

	/**
	 * @brief procedure (built-in function)
	 */
	class Procedure {
	private:
		Scope _scope;
		std::string _name;
		OS::GCRef<Var> _doc;
	public:
		Procedure(const std::string & name);
		virtual ~Procedure();
		Scope & scope();
		std::string & name();
		OS::GCRef<Var> & doc();
		virtual OS::GCRef<Var> proc(Env & env, Scope & scope, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args) = 0;
	};

	/**
	 * @brief 
	 */
	class Boolean {
	private:
		bool _val;
	public:
		Boolean() : _val(false) {}
		Boolean(bool val) : _val(val) {}
		virtual ~Boolean() {}
		bool & val() { return _val; }
		bool val() const { return _val; }
		Boolean & operator= (bool val) { _val = val;  return *this; }
		std::string toString() const { return (_val ? "T" : "NIL"); }
	};

	/**
	 * @brief lisp integer
	 */
    class Integer {
    private:
        long long num;
    public:
        Integer() : num(0) {}
		Integer(short num) : num(num) {}
		Integer(int num) : num(num) {}
		Integer(long num) : num(num) {}
        Integer(long long num) : num(num) {}
        virtual ~Integer() {}

		static bool isIntegerString(const std::string & istr) {
			size_t f = 0;
			if (*istr.begin() == '-' || *istr.begin() == '+') {
				f = 1;
			}
			return (istr.length() - f > 0) &&
				(istr.find_first_not_of("0123456789", f) == std::string::npos);
		}

		static long long toInteger(const std::string & istr) {
			size_t f = (*istr.begin() == '-' || *istr.begin() == '+') ? 1 : 0;
			bool negative = (*istr.begin() == '-');
			long long n = 0;
			for (size_t i = f; i < istr.length(); i++) {
				n *= 10;
				n += istr[i] - '0';
			}
			return (negative ? -n : n);
		}

		long long raw() const {return num;}
		
        long long & operator* () {return num;}
        long long getInteger() const {return num;}
        Integer & operator+=(const Integer & other) {num += other.num; return *this;}
        Integer & operator-=(const Integer & other) {num -= other.num; return *this;}
        Integer & operator*=(const Integer & other) {num *= other.num; return *this;}
        Integer & operator/=(const Integer & other) {num /= other.num; return *this;}
		Integer & operator%=(const Integer & other) {num %= other.num; return *this;}
        
        Integer operator+ (const Integer & other) const {return Integer(num + other.num);}
        Integer operator- (const Integer & other) const {return Integer(num - other.num);}
        Integer operator* (const Integer & other) const {return Integer(num * other.num);}
        Integer operator/ (const Integer & other) const {return Integer(num / other.num);}
		Integer operator% (const Integer & other) const {return Integer(num % other.num);}

		bool operator> (const Integer & other) const {return num > other.num;}
		bool operator< (const Integer & other) const {return num < other.num;}
		bool operator>= (const Integer & other) const {return num >= other.num;}
		bool operator<= (const Integer & other) const {return num <= other.num;}
        bool operator== (const Integer & other) const {return num == other.num;}
        bool operator!= (const Integer & other) const {return num != other.num;}
    };

	/**
	 * @brief lisp float
	 */
	class Float {
	private:
		double num;
	public:
		Float() : num(0.f) {}
		Float(float num) : num((double)num) {}
		Float(double num) : num(num) {}
		virtual ~Float() {}

		double raw() const {return num;}

		static bool isFloatString(const std::string & istr) {
			std::string n = istr;
			if (*istr.begin() == '-' || *istr.begin() == '+') {
				n = n.substr(1);
			}
			if (n.find_first_not_of("0123456789.") != std::string::npos) {
				return false;
			}
			if (n.find(".") == std::string::npos) {
				return false;
			}
			if (n.find(".", n.find(".") + 1) != std::string::npos) {
				return false;
			}
			return true;
		}

		static double toFloat(const std::string & istr) {
			return (double)atof(istr.c_str());
		}

		double & operator* () {return num;}
        Float & operator+=(const Float & other) {num += other.num; return *this;}
        Float & operator-=(const Float & other) {num -= other.num; return *this;}
        Float & operator*=(const Float & other) {num *= other.num; return *this;}
        Float & operator/=(const Float & other) {num /= other.num; return *this;}
        
        Float operator+ (const Float & other) const {return Float(num + other.num);}
        Float operator- (const Float & other) const {return Float(num - other.num);}
        Float operator* (const Float & other) const {return Float(num * other.num);}
        Float operator/ (const Float & other) const {return Float(num / other.num);}

		bool operator> (const Float & other) const {return num > other.num;}
		bool operator< (const Float & other) const {return num < other.num;}
		bool operator>= (const Float & other) const {return num >= other.num;}
		bool operator<= (const Float & other) const {return num <= other.num;}
        bool operator== (const Float & other) const {return num == other.num;}
        bool operator!= (const Float & other) const {return num != other.num;}
	};

	/**
	 * @brief lisp file descriptor
	 */
	class FileDescriptor {
	private:
		FILE * _fd;
	public:
		FileDescriptor() : _fd(NULL) {}
		FileDescriptor(FILE * _fd) : _fd(_fd) {}
		virtual ~FileDescriptor() {}
		FILE * fd() {return _fd;}
		void testFd() {
			if (!_fd) {
				throw LispException("nil file descriptor");
			}
		}
		bool eof() {
			testFd();
			return feof(_fd) ? true : false;
		}
		std::string readline() {
			testFd();
			char buffer[1024] = {0,};
			if (fgets(buffer, sizeof(buffer), _fd)) {
				if (buffer[strlen(buffer) - 1] == '\n') {
					buffer[strlen(buffer) - 1] = '\0';
				}
			}
			return std::string(buffer);
		}
		void write(const std::string & data) {
			testFd();
			fputs(data.c_str(), _fd);
		}
		size_t position() {
			long pos = ftell(_fd);
			if (pos < 0) {
				throw LispException("ftell() error");
			}
			return (size_t)pos;
		}
		void position(size_t seek) {
			fseek(_fd, seek, SEEK_SET);
		}
		void close() {
			if (_fd) {
				fclose(_fd);
				_fd = NULL;
			}
		}
	};

	/**
	 * @brief env
	 */
	class Env {
	private:
		static bool _debug;
		// Env * _parent;
		// std::map<std::string, OS::GCRef<Var> > _vars;
		Scope _scope;
		OS::SharedHeap<Var> _heap;
	public:
		Env();
		// Env(Env * parent);
		virtual ~Env();
		static void setDebug(bool debug);
		void _trace(const std::string & msg);
		Scope & scope();
		OS::SharedHeap<Var> & heap();
		OS::GCRef<Var> alloc(Var * var);
		void gc();
		void clear();
	};

	/**
	 * @brief func
	 */
	class Func {
	private:
		Scope _scope;
		OS::GCRef<Var> _doc;
		OS::GCRef<Var> _params;
		OS::GCRef<Var> _body;
	public:
		Func();
		Func(const OS::GCRef<Var> & params, const OS::GCRef<Var> & body);
		virtual ~Func();
		Scope & scope();
		OS::GCRef<Var> & doc();
		OS::GCRef<Var> & params();
		OS::GCRef<Var> & body();
		const OS::GCRef<Var> doc() const;
		const OS::GCRef<Var> params() const;
		const OS::GCRef<Var> body() const;
		bool empty();
	};

	/**
	 * @brief Var
	 */
	class Var {
	public:
		const static int NIL = 0;
		const static int SYMBOL = 1;
		const static int LIST = 2;
		const static int BOOLEAN = 3;
		const static int INTEGER = 4;
		const static int FLOAT = 5;
		const static int STRING = 6;
		const static int FUNC = 7;
		const static int FILE = 8;
		const static int FILE_DESCRIPTOR = 9;
		
	private:
		static bool _debug;
		int _type;
		std::string _symbol;
		std::string _str;
		std::vector<OS::GCRef<Var> > _lst;
		Boolean _bval;
        Integer _inum;
		Float _fnum;
		Func _func;
		UTIL::AutoRef<Procedure> _procedure;
		OS::File _file;
		FileDescriptor _fd;
		
	public:
		explicit Var();
		explicit Var(const char * token);
		explicit Var(const std::string & token);
		explicit Var(std::vector<OS::GCRef<Var> > lst);
		explicit Var(bool bval);
		explicit Var(const Boolean & bval);
		explicit Var(short inum);
		explicit Var(int inum);
		explicit Var(long inum);
		explicit Var(long long inum);
		explicit Var(const Integer & inum);
		explicit Var(float dnum);
		explicit Var(double dnum);
		explicit Var(const Float & fnum);
		explicit Var(const Func & func);
		explicit Var(UTIL::AutoRef<Procedure> procedure);
		explicit Var(OS::File & file);
		explicit Var(const FileDescriptor & fd);
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
		bool isSymbol() const;
		bool isBoolean() const;
		bool isInteger() const;
		bool isFloat() const;
		bool isString() const;
		bool isFunction() const;
		bool isFile() const;
		bool isFileDescriptor() const;
		std::string & r_symbol();
		std::string & r_string();
		std::vector<OS::GCRef<Var> > & r_list();
		Boolean & r_boolean();
		Integer & r_integer();
		Float & r_float();
		OS::File & r_file();
		Func & r_func();
		UTIL::AutoRef<Procedure> & r_procedure();
		FileDescriptor & r_fileDescriptor();
		OS::GCRef<Var> proc(Env & env, Scope & scope, std::vector<OS::GCRef<Var> > & args);
		OS::GCRef<Var> proc(Env & env, Scope & scope, OS::GCRef<Var> name, std::vector<OS::GCRef<Var> > & args);
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
	 * @brief Arguments
	 */
	class Arguments {
	private:
		std::vector<OS::GCRef<Var> > proto;
		std::map<std::string, OS::GCRef<Var> > _keywords;
	public:
		Arguments();
		Arguments(std::vector<OS::GCRef<Var> > & proto);
		virtual ~Arguments();

		size_t countPartArguments(std::vector<OS::GCRef<Var> > & arr, size_t start);
		void mapArguments(Env & env,
						  Scope & sub_scope,
						  Scope & scope,
						  std::vector<OS::GCRef<Var> > & args);
		size_t mapOptionals(Env & env,
							Scope & sub_scope,
							Scope & scope,
							std::vector<OS::GCRef<Var> > & proto,
							size_t pstart,
							std::vector<OS::GCRef<Var> > & args,
							size_t astart);
		static std::vector<OS::GCRef<Var> > extractRest(Env & env,
														Scope & scope,
														std::vector<OS::GCRef<Var> > & args,
														size_t start);
		static std::map<std::string, OS::GCRef<Var> > extractKeywords(std::vector<OS::GCRef<Var> > & args);
		std::map<std::string, OS::GCRef<Var> > & keywords();
	};

	/**
	 * @brief lisp extern
	 */
	extern OS::GCRef<Var> pathname(Env & env, OS::GCRef<Var> path);
	extern void native(Env & env);
	extern void repl(Env & env);
	extern std::vector<std::string> tokenize(const std::string & s);
	extern OS::GCRef<Var> parse(Env & env, const std::string & cmd);
	extern OS::GCRef<Var> eval(Env & env, Scope & scope, const OS::GCRef<Var> & var);
	extern OS::GCRef<Var> compile(Env & env, const std::string & cmd);
}

#endif
