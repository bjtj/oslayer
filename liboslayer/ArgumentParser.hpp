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
	class ArgumentParser {
	private:
		std::string programName;
		std::vector<Variable> vars;
		std::vector<std::string> texts;
	public:
		ArgumentParser();
		virtual ~ArgumentParser();

		void clear();
		void parse(int argc, char * args[]);
		void parse(int argc, const char * args[]);
		void parse(const std::vector<std::string> & args);
		std::string trimIndicator(const std::string & token);
		bool testLongIndicator(const std::string & token);
		bool testShortIndicator(const std::string & token);
		bool testText(const std::string & token);
		bool testInlineSetter(const std::string & token);
		bool hasVarWithName(const std::string & name);
		bool hasVarWithAlias(const std::string & alias);
		bool hasVarWithShortAlias(const std::string & shortAlias);
		bool hasVarWithIndicator(const std::string & indicator);
		Variable & varWithName(const std::string & name);
		Variable & varWithAlias(const std::string & alias);
		Variable & varWithShortAlias(const std::string & shortAlias);
		Variable & varWithIndicator(const std::string & indicator);
		Variable & var(const std::string & name, const std::string & alias, const std::string & shortAlias);
		Variable & var(const std::string & name);
		std::vector<std::string> getTexts();
		std::string text(size_t idx);
		std::string text(size_t idx, const std::string & def);
		std::string getProgramName();
	};
}

#endif
