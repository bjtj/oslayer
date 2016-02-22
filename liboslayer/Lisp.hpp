#ifndef __LISP_HPP__
#define __LISP_HPP__

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "os.hpp"
#include "AutoRef.hpp"

namespace LISP {

	class Env;
	class Var;

	typedef Var (*fn_proc)(Var name, std::vector<Var> & args, Env & env);
	
	extern std::string text(const std::string & txt);
	extern std::string untext(const std::string & txt);

	class Procedure {
	private:
		std::string name;
	public:
		Procedure(const std::string & name) : name(name) {}
		virtual ~Procedure() {}
		virtual Var proc(Var name, std::vector<Var> & args, Env & env) = 0;
		std::string getName() const {return name;}
	};
    
    class Integer {
    private:
        long long num;
    public:
        Integer() : num(0) {}
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
		const static int PAIR = 9;
		const static int REF_LIST = 10;
	private:
		int type;
		std::string symbol;
		std::string str;
		std::vector<Var> lst;
		std::vector<Var*> rlst;
		bool bval;
        Integer inum;
		float fnum;
		std::vector<Var> params;
		std::vector<Var> body;
		UTIL::AutoRef<Procedure> procedure;
		OS::File file;
		std::vector<Var> conscell;
	public:
		Var() : type(NIL), bval(false), fnum(0) {}
		Var(std::string token) : type(NIL), fnum(0) {
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
			} else if (*token.begin() == '#' && *(token.begin() + 1) == 'p') {
				type = FILE;
				file = OS::File(token.substr(3, token.length() - 4));
			} else {
				type = SYMBOL;
				symbol = token;
			}
		}
		Var(std::vector<Var> lst) : type(LIST), lst(lst), bval(false), fnum(0) {}
		Var(bool bval) : type(BOOLEAN), bval(bval), fnum(0) {}
		Var(Integer inum) : type(INTEGER), bval(false), inum(inum), fnum(0) {}
		Var(float fnum) : type(FLOAT), bval(false), fnum(fnum) {}
		Var(std::vector<Var> params, std::vector<Var> body) : type(FUNC), bval(false), fnum(0), params(params), body(body) {}
		Var(UTIL::AutoRef<Procedure> procedure) : type(FUNC), bval(false), fnum(0), procedure(procedure) {}
		Var(OS::File & file) : type(FILE), bval(false), fnum(0), file(file) {}
		Var(Var cons, Var cell) : type(PAIR), bval(false), fnum(0) {
			conscell.clear();
			conscell.push_back(cons);
			conscell.push_back(cell);
		}
		Var(std::vector<Var*> rlst) : type(REF_LIST), bval(false), fnum(0), rlst(rlst) {}
		virtual ~Var() {}
		
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
			case PAIR:
				return "PAIR";
			case REF_LIST:
				return "REFERENCE LIST";
			default:
				break;
			}
			throw "unknown variable type";
		}
		void checkTypeThrow(int t) const {
			if (type != t) {
				throw "type not match (type: " + getTypeString() +
					", but required: " + getTypeString(t) + ")";
			}
		}
		bool nil() const {return type == NIL;}
		bool isList() const {return type == LIST;}
		bool isSymbol() const {return type == SYMBOL;}
		bool isBoolean() const {return type == BOOLEAN;}
		bool isInteger() const {return type == INTEGER;}
		bool isFloat() const {return type == FLOAT;}
		bool isString() const {return type == STRING;}
		bool isFunction() const {return type == FUNC;}
		bool isFile() const {return type == FILE;}
		bool isPair() const {return type == PAIR;}
		bool isRefList() const {return type == REF_LIST;}
		std::string getSymbol() const {checkTypeThrow(SYMBOL); return symbol;}
		std::string getString() const {checkTypeThrow(STRING); return str;}
		std::vector<Var> & getList() {checkTypeThrow(LIST); return lst;}
		bool getBoolean() {checkTypeThrow(BOOLEAN); return bval;}
		Integer getInteger() {checkTypeThrow(INTEGER); return inum;}
		float getFloat() {checkTypeThrow(FLOAT); return fnum;}
		OS::File & getFile() {checkTypeThrow(FILE); return file;}
		Var getParams() {checkTypeThrow(FUNC); return Var(params);}
		Var getBody() {checkTypeThrow(FUNC); return Var(body);}
		Var & getCons() {checkTypeThrow(PAIR); return conscell[0];}
		Var & getCell() {checkTypeThrow(PAIR); return conscell[1];}
		UTIL::AutoRef<Procedure> getProcedure() {checkTypeThrow(FUNC); return procedure;}
		std::vector<Var*> & getRefList() {checkTypeThrow(REF_LIST); return rlst;}
		virtual Var proc(std::vector<Var> & args, Env & env) {
			if (!procedure.nil()) {
				return proc(procedure->getName(), args, env);
			} else {
				return proc("", args, env);
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
					snprintf(buffer, sizeof(buffer), "%f", fnum);
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
					return "#(PARAMS:" + p.toString() + ", BODY:" + b.toString() + ")";
				}
			case FILE:
				return "#p\"" + file.getPath() + "\"";
			case PAIR:
				return "(" + conscell[0].toString() + " . " + conscell[1].toString() + ")";
			case REF_LIST:
				{
					std::string ret = "(";
					for (std::vector<Var*>::const_iterator iter = rlst.begin(); iter != rlst.end(); iter++) {
						if (iter != rlst.begin()) {
							ret += " ";
						}
						ret += (*iter)->toString();
					}
					ret += ")";
					return ret;
				}
			default:
				break;
			}
			throw "unknown variable type";
		}

		Var & operator= (const Var & other) {
			
			this->symbol = other.symbol;
			this->str = other.str;
			this->lst = other.lst;
			this->bval = other.bval;
			this->inum = other.inum;
			this->fnum = other.fnum;
			this->params = other.params;
			this->body = other.body;
			this->procedure = other.procedure;
			this->file = other.file;
			this->conscell = other.conscell;

			if (this->type == REF_LIST && other.type == LIST) {
				std::vector<Var>::const_iterator oi = other.lst.begin();
				for (std::vector<Var*>::iterator iter = rlst.begin(); iter != rlst.end() && oi != other.lst.end(); iter++, oi++) {
					*(*iter) = *oi;
				}
			} else {
				this->type = other.type;
			}
			return *this;
		}
	};

	class Env {
	private:
		Env * parent;
		bool _quit;
		std::map<std::string, Var> _vars;
	public:
		Env() : parent(NULL), _quit(false) {}
		Env(Env * parent) : parent(parent), _quit(false) {}
		virtual ~Env() {}
		Var & operator[] (const std::string & name) {
			if (parent && _vars.find(name) == _vars.end()) {
				return (*parent)[name];
			}
			return _vars[name];
		}
		Var & local(const std::string & name) {
			return _vars[name];
		}
		void quit(bool q) {
			_quit = q;
			if (parent) {
				parent->quit(q);
			}
		}
		bool quit() {return _quit;}
		std::string toString() {
			std::string ret;
			for (std::map<std::string, Var>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
				ret.append(iter->first + " : " + iter->second.toString());
			}
			return ret;
		}
	};

	extern Var pathname(Var path);
	extern void native(Env & env);
	extern void repl(Env & env);
	extern Var parse(const std::string & cmd);
	extern Var eval(Var & var, Env & env);
}

#endif
