#ifndef __LISP_HPP__
#define __LISP_HPP__

#include <iostream>
#include <string>
#include <map>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>

#include "AutoRef.hpp"

namespace LISP {

	class Env;
	class Var;

	typedef Var (*fn_proc)(std::vector<Var> & args, Env & env);

	class Procedure {
	private:
	public:
		Procedure() {}
		virtual ~Procedure() {}
		virtual Var proc(std::vector<Var> & args, Env & env) = 0;
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
		virtual ~Var() {}
		int getType() { return type; }
		std::string getTypeString() {
			switch (type) {
			case NIL:
				return "nil";
			case SYMBOL:
				return "symbol";
			case LIST:
				return "list";
			case BOOLEAN:
				return "boolean";
			case INTEGER:
				return "integer";
			case FLOAT:
				return "float";
			case STRING:
				return "string";
			case FUNC:
				return "function";
			default:
				break;
			}
			throw "unknown variable type";
		}
		bool nil() {return type == NIL;}
		bool isList() {return type == LIST;}
		bool isSymbol() {return type == SYMBOL;}
		bool isBoolean() {return type == BOOLEAN;}
		bool isInteger() {return type == INTEGER;}
		bool isFloat() {return type == FLOAT;}
		bool isString() {return type == STRING;}
		bool isFunction() {return type == FUNC;}
		std::string getSymbol() {return symbol;}
		std::string getString() {return str;}
		std::vector<Var> & getList() {return lst;}
		bool getBoolean() {return bval;}
		int getInteger() {return inum;}
		float getFloat() {return fnum;}
		Var getParams() {return Var(params);}
		Var getBody() {return Var(body);}
		virtual Var proc(std::vector<Var> & args, Env & env);
		std::string toString() {
			switch (type) {
			case NIL:
				return "'nil";
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
				return bval ? "true" : "false";
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
					Var p(params);
					Var b(body);
					return "#(PARAMS:" + p.toString() + ", BODY:" + b.toString() + ")";
				}
			default:
				break;
			}
			throw "unknown variable type";
		}
	};

	class Env {
	private:
		bool _quit;
		std::map<std::string, Var> _vars;
	public:
		Env() : _quit(false) {}
		virtual ~Env() {}
		Var & operator[] (const std::string & name) {
			return _vars[name];
		}
		void quit(bool q) {_quit = q;}
		bool quit() {return _quit;}
		std::string toString() {
			for (std::map<std::string, Var>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
				std::cout << iter->first << " : " << iter->second.toString() << std::endl;
			}
		}
	};

	extern void native(Env & env);
	extern void repl(Env & env);
	extern Var parse(const std::string & cmd);
	extern Var eval(Var & var, Env & env);
}

#endif
