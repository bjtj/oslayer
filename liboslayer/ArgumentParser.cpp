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


	/**
	 * @brief 
	 */

	Arguments::Arguments() {
	}

	Arguments::~Arguments() {
	}

	void Arguments::clear() {
		_programName.clear();
		_vars.clear();
		_texts.clear();
	}

	string & Arguments::programName() {
		return _programName;
	}

	vector<string> & Arguments::texts() {
		return _texts;
	}
	
	string & Arguments::text(size_t idx) {
		return _texts[idx];
	}

	string Arguments::varAsString(const string & any, const string & def) {
		if (!hasVarWithAny(any)) {
			return def;
		}
		return varWithAny(any).value();
	}
	
	bool Arguments::varAsBoolean(const string & any, bool def) {
		if (!hasVarWithAny(any)) {
			return def;
		}
		return varWithAny(any).valueAsBoolean();
	}
	
	int Arguments::varAsInteger(const string & any, int def) {
		if (!hasVarWithAny(any)) {
			return def;
		}
		return varWithAny(any).valueAsInteger();
	}

	bool Arguments::hasVarWithAny(const string & any) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->name() == any || iter->alias() == any || iter->shortAlias() == any) {
				return true;
			}
		}
		return false;
	}
	
	bool Arguments::hasVarWithName(const string & name) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->name() == name) {
				return true;
			}
		}
		return false;
	}
	bool Arguments::hasVarWithAlias(const string & alias) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->alias() == alias) {
				return true;
			}
		}
		return false;
	}
	bool Arguments::hasVarWithShortAlias(const string & shortAlias) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->shortAlias() == shortAlias) {
				return true;
			}
		}
		return false;
	}

	Variable & Arguments::varWithAny(const string & any) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->name() == any || iter->alias() == any || iter->shortAlias() == any) {
				return *iter;
			}
		}
		throw Exception("not found var with - '" + any + "'");
	}
	
	Variable & Arguments::varWithName(const string & name) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->name() == name) {
				return *iter;
			}
		}
		throw Exception("not found var with name - '" + name + "'");
	}
	
	Variable & Arguments::varWithAlias(const string & alias) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->alias() == alias) {
				return *iter;
			}
		}
		throw Exception("not found var with alias - '" + alias + "'");
	}
	
	Variable & Arguments::varWithShortAlias(const string & shortAlias) {
		for (vector<Variable>::iterator iter = _vars.begin(); iter != _vars.end(); iter++) {
			if (iter->shortAlias() == shortAlias) {
				return *iter;
			}
		}
		throw Exception("not found var with short alias - '" + shortAlias + "'");
	}
	
	Variable & Arguments::obtainVar(const string & name, const string & alias, const string & shortAlias) {
		if (hasVarWithName(name)) {
			Variable & v = varWithName(name);
			v.alias() = alias;
			v.shortAlias() = shortAlias;
			return v;
		}
		_vars.push_back(Variable(name, alias, shortAlias));
		return *_vars.rbegin();
	}
	
	Variable & Arguments::obtainVar(const string & name) {
		if (hasVarWithName(name)) {
			return varWithName(name);
		}
		_vars.push_back(Variable(name));
		return *_vars.rbegin();
	}


	/**
	 * @brief 
	 */
	
	ArgumentParser::ArgumentParser() {
	}
	
	ArgumentParser::~ArgumentParser() {
	}
	
	Arguments ArgumentParser::parse(int argc, char * args[]) {
		vector<string> lst;
		for (int i = 0; i < argc; i++) {
			lst.push_back(string(args[i]));
		}
		return parse(lst);
	}
	
	Arguments ArgumentParser::parse(int argc, const char * args[]) {
		vector<string> lst;
		for (int i = 0; i < argc; i++) {
			lst.push_back(string(args[i]));
		}
		return parse(lst);
	}
	
	Arguments ArgumentParser::parse(const vector<string> & args) {
		Arguments ret;
		ret.programName() = args[0];
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

				string indicatorName = getIndicatorName(indicator);
				
				if (testLongIndicator(tok) && ret.hasVarWithAlias(indicatorName)) {
					ret.varWithAlias(indicatorName) = value;
				} else if (testShortIndicator(tok) && ret.hasVarWithShortAlias(indicatorName)) {
					ret.varWithShortAlias(indicatorName) = value;
				} else {
					string alias = trimIndicator(indicator);
					ret.obtainVar(alias, alias, alias) = value;
				}
			} else {
				ret.texts().push_back(tok);
			}
		}
		return ret;
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

	string ArgumentParser::getIndicatorName(const string & indicator) {
		if (testLongIndicator(indicator)) {
			return indicator.substr(2);
		} else if (testShortIndicator(indicator)) {
			return indicator.substr(1);
		}
		throw Exception("Unknown indicator type");
	}
}
