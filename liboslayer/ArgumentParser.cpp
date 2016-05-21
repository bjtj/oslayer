#include "os.hpp"
#include "ArgumentParser.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;
	
	Variable::Variable() {
	}
	Variable::Variable(const string & name) : _name(name), _alias(name), _shortAlias(name.substr(0, 1)) {
	}
	Variable::Variable(const string & name, const string & alias, const string & shortAlias) : _name(name), _alias(alias), _shortAlias(shortAlias) {
	}
	Variable::~Variable() {
	}

	string & Variable::name() {
		return _name;
	}
	string & Variable::alias() {
		return _alias;
	}
	string & Variable::shortAlias() {
		return _shortAlias;
	}
	string & Variable::value() {
		return _value;
	}
	bool Variable::valueAsBoolean() {
		if (_value.empty()) {
			return false;
		}

		if (Text::equalsIgnoreCase(_value, "y") ||
			Text::equalsIgnoreCase(_value, "yes") ||
			Text::equalsIgnoreCase(_value, "true") ||
			Text::toInt(_value) > 0) {
			return true;
		}

		return false;
	}
	int Variable::valueAsInteger() {
		return Text::toInt(_value);
	}
	float Variable::valueAsFloat() {
		return Text::toFloat(_value);
	}
	void Variable::operator=(const string & value) {
		_value = value;
	}
	void Variable::operator=(const char * value) {
		_value = string(value);
	}
	void Variable::operator=(bool bval) {
		_value = (bval ? "yes" : "no");
	}
	void Variable::operator=(int ival) {
		_value = Text::toString(ival);
	}
	void Variable::operator=(float fval) {
		_value = Text::toString(fval);
	}


	
	ArgumentParser::ArgumentParser() {
	}
	ArgumentParser::~ArgumentParser() {
	}
	void ArgumentParser::clear() {
		programName.clear();
		vars.clear();
		texts.clear();
	}
	void ArgumentParser::parse(int argc, char * args[]) {
		vector<string> lst;
		for (int i = 0; i < argc; i++) {
			lst.push_back(string(args[i]));
		}
		parse(lst);
	}
	void ArgumentParser::parse(int argc, const char * args[]) {
		vector<string> lst;
		for (int i = 0; i < argc; i++) {
			lst.push_back(string(args[i]));
		}
		parse(lst);
	}
	void ArgumentParser::parse(const vector<string> & args) {

		programName = args[0];
		for (vector<string>::const_iterator iter = args.begin() + 1; iter != args.end(); iter++) {
			string tok = *iter;
			if (testLongIndicator(tok) || testShortIndicator(tok)) {

				string indicator = tok;
				string value;

				if (testInlineSetter(tok)) {
					indicator = tok.substr(0, tok.find("="));
					value = tok.substr(tok.find("=") + 1);
				} else if ((iter + 1) == args.end() || !testText(*(iter + 1))) {
					value = "yes";
				} else {
					value = *(++iter);
				}

				if (hasVarWithIndicator(indicator)) {
					Variable & var = varWithIndicator(indicator);
					var = value;
				} else {
					string alias = trimIndicator(indicator);
					var(alias, alias, alias) = value;
				}
			} else {
				texts.push_back(tok);
			}
		}
	}
	string ArgumentParser::trimIndicator(const string & token) {
		if (testLongIndicator(token)) {
			return token.substr(2);
		} else if (testShortIndicator(token)) {
			return token.substr(1);
		}
		return token;
	}
	bool ArgumentParser::testLongIndicator(const string & token) {
		return Text::startsWith(token, "--");
	}
	bool ArgumentParser::testShortIndicator(const string & token) {
		return Text::startsWith(token, "-") && !Text::startsWith(token, "--");
	}
	bool ArgumentParser::testText(const string & token) {
		return !testLongIndicator(token) && !testShortIndicator(token);
	}
	bool ArgumentParser::testInlineSetter(const string & token) {
		if (testLongIndicator(token) || testShortIndicator(token)) {
			return token.find("=") != string::npos;
		}
		return false;
	}
	bool ArgumentParser::hasVarWithName(const string & name) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->name() == name) {
				return true;
			}
		}
		return false;
	}
	bool ArgumentParser::hasVarWithAlias(const string & alias) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->alias() == alias) {
				return true;
			}
		}
		return false;
	}
	bool ArgumentParser::hasVarWithShortAlias(const string & shortAlias) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->shortAlias() == shortAlias) {
				return true;
			}
		}
		return false;
	}
	bool ArgumentParser::hasVarWithIndicator(const string & indicator) {
		if (testLongIndicator(indicator)) {
			string alias = indicator.substr(2);
			return hasVarWithAlias(alias);
		} else if (testShortIndicator(indicator)) {
			string shortAlias = indicator.substr(1);
			return hasVarWithShortAlias(shortAlias);
		}
		return false;
	}
	Variable & ArgumentParser::varWithName(const string & name) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->name() == name) {
				return *iter;
			}
		}
		throw Exception("not found var with name - '" + name + "'");
	}
	Variable & ArgumentParser::varWithAlias(const string & alias) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->alias() == alias) {
				return *iter;
			}
		}
		throw Exception("not found var with alias - '" + alias + "'");
	}
	Variable & ArgumentParser::varWithShortAlias(const string & shortAlias) {
		for (vector<Variable>::iterator iter = vars.begin(); iter != vars.end(); iter++) {
			if (iter->shortAlias() == shortAlias) {
				return *iter;
			}
		}
		throw Exception("not found var with short alias - '" + shortAlias + "'");
	}
	Variable & ArgumentParser::varWithIndicator(const string & indicator) {
		if (testLongIndicator(indicator)) {
			string alias = indicator.substr(2);
			return varWithAlias(alias);
		} else if (testShortIndicator(indicator)) {
			string shortAlias = indicator.substr(1);
			return varWithShortAlias(shortAlias);
		}
		throw Exception("wrong indicator - '" + indicator + "'");
	}
	Variable & ArgumentParser::var(const string & name, const string & alias, const string & shortAlias) {
		if (hasVarWithName(name)) {
			Variable & v = varWithName(name);
			v.alias() = alias;
			v.shortAlias() = shortAlias;
			return v;
		}
		vars.push_back(Variable(name, alias, shortAlias));
		return *vars.rbegin();
	}
	Variable & ArgumentParser::var(const string & name) {
		if (hasVarWithName(name)) {
			return varWithName(name);
		}
		vars.push_back(Variable(name));
		return *vars.rbegin();
	}
	vector<string> ArgumentParser::getTexts() {
		return texts;
	}
	string ArgumentParser::text(size_t idx) {
		return texts[idx];
	}
	string ArgumentParser::text(size_t idx, const string & def) {
		if (idx >= texts.size()) {
			return def;
		}
		return text(idx);
	}
	string ArgumentParser::getProgramName() {
		return programName;
	}
}
