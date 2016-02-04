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

	class Procedure {
	private:
		std::string name;
	public:
		Procedure(const std::string & name) : name(name) {}
		virtual ~Procedure() {}
		virtual Var proc(Var name, std::vector<Var> & args, Env & env) = 0;
		std::string & getName() {return name;}
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
	private:
		int type;
		std::string symbol;
		std::string str;
		std::vector<Var> lst;
		bool bval;
		int inum;
		float fnum;
		std::vector<Var> params;
		std::vector<Var> body;
		UTIL::AutoRef<Procedure> procedure;
		OS::File file;
	public:
		Var() : type(NIL), bval(false), inum(0), fnum(0) {}
		Var(std::string token) : type(NIL), inum(0), fnum(0) {
			if (token == "nil") {
				type = NIL;
			} else if (token == "t" || token == "f") {
				type = BOOLEAN;
				bval = (token == "t");
			} else if (*token.begin() == '\"' && *token.rbegin() == '\"') {
				type = STRING;
				str = token;
			} else if (token.find_first_not_of("0123456789") == std::string::npos) {
				type = INTEGER;
				inum = atoi(token.c_str());
			} else if (*token.begin() == '#' && *(token.begin() + 1) == 'p') {
				type = FILE;
				file = OS::File(token.substr(3, token.length() - 4));
			} else {
				type = SYMBOL;
				symbol = token;
			}
		}
		Var(std::vector<Var> lst) : type(LIST), lst(lst), bval(false), inum(0), fnum(0) {}
		Var(bool bval) : type(BOOLEAN), bval(bval), inum(0), fnum(0) {}
		Var(int inum) : type(INTEGER), bval(false), inum(inum), fnum(0) {}
		Var(float fnum) : type(FLOAT), bval(false), inum(0), fnum(fnum) {}
		Var(std::vector<Var> params, std::vector<Var> body) : type(FUNC), bval(false), inum(0), fnum(fnum), params(params), body(body) {}
		Var(UTIL::AutoRef<Procedure> procedure) : type(FUNC), bval(false), inum(0), fnum(0), procedure(procedure) {}
		Var(OS::File & file) : type(FILE), bval(false), inum(0), fnum(0), file(file) {}
		virtual ~Var() {}
		int getType() { return type; }
		std::string getTypeString() {
			return getTypeString(type);
		}
		std::string getTypeString(int type) {
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
			default:
				break;
			}
			throw "unknown variable type";
		}
		void checkTypeThrow(int t) {
			if (type != t) {
				throw "type not match (type: " + getTypeString() +
					", but required: " + getTypeString(t) + ")";
			}
		}
		bool nil() {return type == NIL;}
		bool isList() {return type == LIST;}
		bool isSymbol() {return type == SYMBOL;}
		bool isBoolean() {return type == BOOLEAN;}
		bool isInteger() {return type == INTEGER;}
		bool isFloat() {return type == FLOAT;}
		bool isString() {return type == STRING;}
		bool isFunction() {return type == FUNC;}
		bool isFile() {return type == FILE;}
		std::string getSymbol() {checkTypeThrow(SYMBOL); return symbol;}
		std::string getString() {checkTypeThrow(STRING); return str;}
		std::vector<Var> & getList() {checkTypeThrow(LIST); return lst;}
		bool getBoolean() {checkTypeThrow(BOOLEAN); return bval;}
		int getInteger() {checkTypeThrow(INTEGER); return inum;}
		float getFloat() {checkTypeThrow(FLOAT); return fnum;}
		OS::File & getFile() {checkTypeThrow(FILE); return file;}
		Var getParams() {checkTypeThrow(FUNC); return Var(params);}
		Var getBody() {checkTypeThrow(FUNC); return Var(body);}
		virtual Var proc(Var name, std::vector<Var> & args, Env & env);
		std::string toString() {
			switch (type) {
			case NIL:
				return "NIL";
			case SYMBOL:
				return symbol;
			case LIST:
				{
					std::string ret = "(";
					for (std::vector<Var>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
						if (iter != lst.begin()) {
							ret += ", ";
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
					snprintf(buffer, sizeof(buffer), "%d", inum);
					return buffer;
				}
			case FLOAT:
				{
					char buffer[1024] = {0,};
					snprintf(buffer, sizeof(buffer), "%f", fnum);
					return buffer;
				}
			case STRING:
				return str.substr(1, str.length() - 2);
			case FUNC:
				{
					if (!procedure.empty()) {
						return "#<COMPILED FUNCTION>";
					}
					Var p(params);
					Var b(body);
					return "#(PARAMS:" + p.toString() + ", BODY:" + b.toString() + ")";
				}
			case FILE:
				return "#p\"" + file.getPath() + "\"";
			default:
				break;
			}
			throw "unknown variable type";
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
			for (std::map<std::string, Var>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
				std::cout << iter->first << " : " << iter->second.toString() << std::endl;
			}
		}
	};

	extern Var pathname(Var path);
	extern std::string text(const std::string & txt);
	extern void native(Env & env);
	extern void repl(Env & env);
	extern Var parse(const std::string & cmd);
	extern Var eval(Var & var, Env & env);
}

#endif
