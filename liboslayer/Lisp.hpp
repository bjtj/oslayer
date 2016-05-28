#ifndef __LISP_HPP__
#define __LISP_HPP__

#include <string>
#include <map>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "os.hpp"
#include "AutoRef.hpp"
#include "Text.hpp"

namespace LISP {

	class Env;
	class Var;

	/**
	 * @brief lisp exception
	 */
	class LispException : public OS::Exception{
	private:
	public:
		explicit LispException() {}
		explicit LispException(const std::string & message) : OS::Exception(message) {}
		explicit LispException(const char * message) : OS::Exception(message) {}
		explicit LispException(const std::string & message, int errorCode, int subErrorCode) : OS::Exception(message, errorCode, subErrorCode) {}
		explicit LispException(const char * message, int errorCode, int subErrorCode) : OS::Exception(message, errorCode, subErrorCode) {}
		virtual ~LispException() throw() {}
	};


	typedef Var (*fn_proc)(Var name, std::vector<Var> & args, Env & env);
	extern std::string text(const std::string & txt);
	extern std::string untext(const std::string & txt);

	/**
	 * @brief procedure (built-in function)
	 */
	class Procedure {
	private:
		std::string name;
	public:
		Procedure(const std::string & name) : name(name) {}
		virtual ~Procedure() {}
		virtual Var proc(Var name, std::vector<Var> & args, Env & env) = 0;
		std::string getName() const {return name;}
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
		float num;
	public:
		Float() : num(0.f) {}
		Float(float num) : num(num) {}
		virtual ~Float() {}

		float raw() const {return num;}

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

		static float toFloat(const std::string & istr) {
			return (float)atof(istr.c_str());
		}

		float & operator* () {return num;}
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
		const static int REF = 9;
		const static int FILE_DESCRIPTOR = 10;
		
	private:
		int type;
		std::string symbol;
		std::string str;
		std::vector<Var> lst;
		bool bval;
        Integer inum;
		Float fnum;
		std::vector<Var> params;
		std::vector<Var> body;
		UTIL::AutoRef<Procedure> procedure;
		OS::File file;
		FileDescriptor fd;
		Var * refvar;
		
	public:
		Var() : type(NIL), bval(false), refvar(NULL) {}
		Var(const char * token) : type(NIL), refvar(NULL) {
			init(std::string(token));
		}
		Var(const std::string & token) : type(NIL), refvar(NULL) {
			init(token);
		}
		Var(std::vector<Var> lst) : type(LIST), lst(lst), bval(false), refvar(NULL) {}
		Var(bool bval) : type(BOOLEAN), bval(bval) {
			if (!bval) {
				type = NIL;
			}
		}
		Var(short inum) : type(INTEGER), bval(false), inum(inum), refvar(NULL) {}
		Var(int inum) : type(INTEGER), bval(false), inum(inum), refvar(NULL) {}
		Var(long inum) : type(INTEGER), bval(false), inum(inum), refvar(NULL) {}
		Var(long long inum) : type(INTEGER), bval(false), inum(inum), refvar(NULL) {}
		Var(Integer inum) : type(INTEGER), bval(false), inum(inum), refvar(NULL) {}
		Var(float fnum) : type(FLOAT), bval(false), fnum(fnum), refvar(NULL) {}
		Var(Float fnum) : type(FLOAT), bval(false), fnum(fnum), refvar(NULL) {}
		Var(std::vector<Var> params, std::vector<Var> body) : type(FUNC), bval(false), params(params), body(body), refvar(NULL) {}
		Var(UTIL::AutoRef<Procedure> procedure) : type(FUNC), bval(false), procedure(procedure), refvar(NULL) {}
		Var(OS::File & file) : type(FILE), bval(false), file(file), refvar(NULL) {}
		Var(FileDescriptor fd) : type(FILE_DESCRIPTOR), bval(false), fd(fd), refvar(NULL) {}
		Var(Var * refvar) : type(REF), bval(false), refvar(refvar) {
			if (refvar == NULL) {
				type = NIL;
			}

			if (refvar->isRef()) {
				this->refvar = refvar->refvar;
			}
		}
		virtual ~Var() {}

		void init(const std::string & token) {
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
		int getType() { return type; }
		std::string getTypeString() const {
			return getTypeString(type);
		}
		std::string getTypeString(int type) const {
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
			throw LispException("unknown variable type / " + UTIL::Text::toString(type));
		}
		void checkTypeThrow(int t) const {
			if (type != t) {
				throw LispException(toString() + " / type not match (type: " + getTypeString() +
									", but required: " + getTypeString(t) + ")");
			}
		}
		bool isNil() const {return type == NIL;}
		bool isList() const {return type == LIST;}
		bool isSymbol() const {return type == SYMBOL;}
		bool isBoolean() const {return type == BOOLEAN;}
		bool isInteger() const {return type == INTEGER;}
		bool isFloat() const {return type == FLOAT;}
		bool isString() const {return type == STRING;}
		bool isFunction() const {return type == FUNC;}
		bool isFile() const {return type == FILE;}
		bool isRef() const {return type == REF;}
		bool isFileDescriptor() const {return type == FILE_DESCRIPTOR;}
		std::string getSymbol() const {checkTypeThrow(SYMBOL); return symbol;}
		std::string getString() const {checkTypeThrow(STRING); return str;}
		std::vector<Var> & getList() {checkTypeThrow(LIST); return lst;}
		bool getBoolean() {checkTypeThrow(BOOLEAN); return bval;}
		Integer getInteger() {checkTypeThrow(INTEGER); return inum;}
		Float getFloat() {checkTypeThrow(FLOAT); return fnum;}
		OS::File & getFile() {checkTypeThrow(FILE); return file;}
		Var getParams() {checkTypeThrow(FUNC); return Var(params);}
		Var getBody() {checkTypeThrow(FUNC); return Var(body);}
		UTIL::AutoRef<Procedure> getProcedure() {checkTypeThrow(FUNC); return procedure;}
		Var * getRef() const {checkTypeThrow(REF); return refvar;}
		FileDescriptor & getFileDescriptor() {checkTypeThrow(FILE_DESCRIPTOR); return fd;}
		virtual Var proc(std::vector<Var> & args, Env & env) {
			if (!procedure.nil()) {
				return proc(procedure->getName(), args, env);
			} else {
				return proc(Var("nil"), args, env);
			}
		}
		virtual Var proc(Var name, std::vector<Var> & args, Env & env);
		std::string toString() const {
			switch (type) {
			case NIL:
				return "NIL";
			case SYMBOL:
				return symbol;
			case LIST:
				{
					std::string ret = "(";
					for (std::vector<Var>::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
						if (iter != lst.begin()) {
							ret += " ";
						}
						ret += iter->toString();
					}
					ret += ")";
					return ret;
				}
			case BOOLEAN:
				return bval ? "T" : "NIL";
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
					Var p(params);
					Var b(body);
					return "#<FUNCTION (PARAMS:" + p.toString() + ", BODY:" + b.toString() + ")>";
				}
			case FILE:
				return "#p\"" + file.getPath() + "\"";
			case REF:
				return "#REFERENCE/" + refvar->toString();
			case FILE_DESCRIPTOR:
				return "#<FD>";
			default:
				break;
			}
			throw LispException("unknown variable type / " + UTIL::Text::toString(type));
		}

		Var & operator* () {
			if (type == REF) {
				return *refvar;
			}
			return *this;
		}

		Var & operator= (const Var & other) {
			
			this->symbol = other.symbol;
			this->str = other.str;
			this->bval = other.bval;
			this->inum = other.inum;
			this->fnum = other.fnum;
			this->params = other.params;
			this->body = other.body;
			this->procedure = other.procedure;
			this->file = other.file;
			this->fd = other.fd;

			if (this->type == LIST && this->lst.size() > 0 && this->lst[0].isRef() && other.type == LIST) {
				std::vector<Var>::const_iterator o = other.lst.begin();
				for (std::vector<Var>::iterator i = this->lst.begin(); i != this->lst.end() && o != other.lst.end(); i++, o++) {
					*i = *o;
				}
			} else {
				this->lst.clear();
				if (other.type == LIST) {
					for (std::vector<Var>::const_iterator iter = other.lst.begin(); iter != other.lst.end(); iter++) {
						this->lst.push_back(iter->isRef() ? *(iter->getRef()) : *iter);
					}
				}
			}

			if (this->type == REF) {
				if (other.type == REF) {
					*(this->refvar) = other.refvar;
				} else {
					*(this->refvar) = other;
				}
			} else if (other.type == REF) {
				(*this) = *(other.refvar);
			} else {
				this->type = other.type;
			}
			return *this;
		}
	};

	/**
	 * @brief env
	 */
	class Env {
	private:
		Env * parent;
		bool _quit;
		std::map<std::string, Var> _vars;
		std::vector<Var> _stack;
	public:
		Env() : parent(NULL), _quit(false) {}
		Env(Env * parent) : parent(parent), _quit(false) {}
		virtual ~Env() {}
		bool find (const std::string & name) {
			if ((_vars.find(name) == _vars.end()) == false) {
				return true;
			}
			if (parent && parent->find(name)) {
				return true;
			}
			return false;
		}
		Var & operator[] (const std::string & name) {
			if (parent && _vars.find(name) == _vars.end()) {
				return (*parent)[name];
			}
			return _vars[name];
		}
		std::map<std::string, Var> & root() {
			if (parent) {
				return parent->local();
			}
			return _vars;
		}
		std::map<std::string, Var> & local() {
			return _vars;
		}
		std::vector<Var> & stack() {
			if (parent) {
				return parent->stack();
			}
			return _stack;
		}
		void push(Var var) {
			stack().push_back(var);
		}
		Var pop() {
			Var ret = *(stack().rbegin());
			stack().erase(stack().begin() + stack().size() - 1);
			return ret;
		}
		Var & last() {
			if (stack().size() == 0) {
				throw LispException("empty stack");
			}
			return *(stack().rbegin());
		}
		void quit(bool q) {
			_quit = q;
			if (parent) {
				parent->quit(q);
			}
		}
		bool quit() {
			return _quit;
		}
		std::string toString() {
			std::string ret;
			for (std::map<std::string, Var>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
				ret.append(iter->first + " : " + iter->second.toString());
			}
			return ret;
		}
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
		static std::string trimComment(const std::string & text);
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
		std::vector<Var> proto;
		std::map<std::string, Var> _keywords;
	public:
		Arguments();
		Arguments(std::vector<Var> & proto);
		virtual ~Arguments();

		size_t countPartArguments(std::vector<Var> & arr, size_t start);
		void mapArguments(Env & env, std::map<std::string, Var> & scope, std::vector<Var> & args);
		size_t mapOptionals(Env & env, std::map<std::string, Var> & scope, std::vector<Var> & proto, size_t pstart, std::vector<Var> & args, size_t astart);
		static std::vector<Var> extractRest(Env & env, std::vector<Var> & args, size_t start);
		static std::map<std::string, Var> extractKeywords(std::vector<Var> & args);
		std::map<std::string, Var> & keywords();
	};


	/**
	 * @brief Iterator
	 */
	template <typename T>
	class Iterator {
	private:
		std::vector<T> & lst;
		typename std::vector<T>::iterator iter;
	public:
		Iterator(std::vector<T> & lst) : lst(lst) {
			iter = lst.begin();
		}
		virtual ~Iterator() {}

		void offset(size_t o) {
			iter += o;
		}

		bool hasNext() {
			return (iter != lst.end());
		}
		T & next() {
			if (!hasNext()) {
				throw LispException("out of bound");
			}
			return *(iter++);
		}
	};

	/**
	 * @brief lisp utility
	 */

	extern Var pathname(Var path);
	extern void native(Env & env);
	extern void repl(Env & env);
	extern std::vector<std::string> tokenize(const std::string & s);
	extern Var parse(const std::string & cmd);
	extern Var refeval(Var & var, Env & env);
	extern Var eval(Var & var, Env & env);
	extern Var compile(const std::string & cmd, Env & env);
}

#endif
