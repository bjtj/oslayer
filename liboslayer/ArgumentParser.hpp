#ifndef __ARGUMENT_PARSER_HPP__
#define __ARGUMENT_PARSER_HPP__

#include <string>
#include <map>
#include <vector>

namespace UTIL {

	/**
	 * @brief 
	 */
	class Variable {
		std::string _name;
		std::string _alias;
		std::string _shortAlias;
		std::string _value;
	public:
		Variable();
		Variable(const std::string & name);
		Variable(const std::string & name, const std::string & alias, const std::string & shortAlias);
		virtual ~Variable();
		std::string & name();
		std::string & alias();
		std::string & shortAlias();
		std::string & value();
		bool valueAsBoolean();
		int valueAsInteger();
		float valueAsFloat();
		void operator=(const std::string & value);
		void operator=(const char * value);
		void operator=(bool bval);
		void operator=(int ival);
		void operator=(float fval);
	};

	/**
	 * @brief
	 */
	class Arguments {
	private:
		std::string _programName;
		std::vector<Variable> _vars;
		std::vector<std::string> _texts;
		
	public:
		Arguments();
		virtual ~Arguments();
		void clear();
		std::string & programName();
		std::vector<std::string> & texts();
		std::string & text(size_t idx);
		std::string varAsString(const std::string & any, const std::string & def);
		bool varAsBoolean(const std::string & any, bool def);
		int varAsInteger(const std::string & any, int def);
		bool hasVarWithAny(const std::string & any);
		bool hasVarWithName(const std::string & name);
		bool hasVarWithAlias(const std::string & alias);
		bool hasVarWithShortAlias(const std::string & shortAlias);
		Variable & varWithAny(const std::string & any);
		Variable & varWithName(const std::string & name);
		Variable & varWithAlias(const std::string & alias);
		Variable & varWithShortAlias(const std::string & shortAlias);
		Variable & obtainVar(const std::string & name, const std::string & alias, const std::string & shortAlias);
		Variable & obtainVar(const std::string & name);
	};
	
	/**
	 * @brief 
	 */
	class ArgumentParser {
	private:
		Arguments _arguments;
	public:
		ArgumentParser();
		ArgumentParser(int argc, char * args[]);
		ArgumentParser(int argc, const char * args[]);
		virtual ~ArgumentParser();
		Arguments parse(int argc, char * args[]);
		Arguments parse(int argc, const char * args[]);
		Arguments parse(const std::vector<std::string> & args);
		std::string trimIndicator(const std::string & token);
		bool testLongIndicator(const std::string & token);
		bool testShortIndicator(const std::string & token);
		bool testText(const std::string & token);
		bool testInlineSetter(const std::string & token);
		std::string getIndicatorName(const std::string & indicator);
		Arguments & arguments();
	};
}

#endif
