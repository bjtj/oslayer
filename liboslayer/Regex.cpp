#include <iostream>
#include "Regex.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

#define LOGD(D,M) if (D) cout << M << endl;
	
	bool ch_in(char ch, const string & group) {
		return (group.find(ch) != string::npos);
	}

	bool contains(const string & tok, const vector<string> & cand) {
		for (vector<string>::const_iterator iter = cand.begin(); iter != cand.end(); iter++) {
			if (tok == *iter) {
				return true;
			}
		}
		return false;
	}

	/**
	 * @brief quantity
	 */
	Quantity::Quantity() : _occured(false), _initial(1), _limit(1) {}
	Quantity::~Quantity() {}
	bool & Quantity::occured() {
		return _occured;
	}
	int & Quantity::initial() {
		return _initial;
	}
	int Quantity::const_initial() const {
		return _initial;
	}
	int & Quantity::limit() {
		return _limit;
	}
	int Quantity::const_limit() const {
		return _limit;
	}
	bool Quantity::testInRange(int i) const {
		return (i >= const_initial() && testInLimit(i));
	}
	bool Quantity::testInLimit(int i) const {
		return (const_limit() < 0 || i <= const_limit());
	}
	bool Quantity::testUnderLimit(int i) const {
		return (const_limit() < 0 || i < const_limit());
	}
	string Quantity::toString() const {
		return "{" + Text::toString(_initial) + ", " + Text::toString(_limit) + "}";
	}

	/**
	 * @brief position
	 */
	Position::Position() : _start_of_line(false), _end_of_line(false) {}
	Position::~Position() {}
	bool & Position::start_of_line() { return _start_of_line; }
	bool & Position::end_of_line() { return _end_of_line; }

	/**
	 * @brief match type
	 */
	const int MatchType::UNMATCHED = 0;
	const int MatchType::FULL_MATCHED = 1;
	const int MatchType::PARTIAL_MATCHED = 2;
	const std::string MatchType::UNMATCHED_STR = "UNMATCHED";
	const std::string MatchType::FULL_MATCHED_STR = "FULL_MATCHED";
	const std::string MatchType::PARTIAL_MATCHED_STR = "PARTIAL_MATCHED";
	
	MatchType::MatchType() : _type(UNMATCHED) {}
	MatchType::MatchType(int type) : _type(type) {}
	MatchType::~MatchType() {}
	string MatchType::toString() const {
		return MatchType::toString(_type);
	}
	bool MatchType::operator== (int type) const {
		return _type == type;
	}
	bool MatchType::operator== (const MatchType & other) const {
		return _type == other._type;
	}
	bool MatchType::operator!= (int type) const {
		return _type != type;
	}
	bool MatchType::operator!= (const MatchType & other) const {
		return _type != other._type;
	}
	string MatchType::toString(int type) {
		switch (type) {
		case UNMATCHED:
			return UNMATCHED_STR;
		case FULL_MATCHED:
			return FULL_MATCHED_STR;
		case PARTIAL_MATCHED:
			return PARTIAL_MATCHED_STR;
		}
		throw OS::Exception("unknown match type - " + UTIL::Text::toString(type));
	}
	MatchType MatchType::toType(const string & type) {
		if (type == UNMATCHED_STR) {
			return MatchType(UNMATCHED);
		} else if (type == FULL_MATCHED_STR) {
			return MatchType(FULL_MATCHED);
		} else if (type == PARTIAL_MATCHED_STR) {
			return MatchType(PARTIAL_MATCHED);
		}
		throw OS::Exception("unknown match type - " + type);
	}

	ostream & operator<<(ostream & stream, const MatchType & type) {
		stream << type.toString();
		return stream;
	}  

	/**
	 * @brief match result
	 */

	MatchResult::MatchResult() : _length(0) {}
	MatchResult::~MatchResult() {}
	MatchType & MatchResult::matchType() { return _matchType; }
	size_t & MatchResult::length() { return _length; }
	vector<string> & MatchResult::groups() { return _groups; }
	void MatchResult::appendGroup(const string & group) {
		_groups.push_back(group);
	}
	void MatchResult::appendGroups(const vector<std::string> & groups) {
		_groups.insert(_groups.end(), groups.begin(), groups.end());
	}

	/**
	 * @brief 
	 */

	bool Matcher::_debug = false;

	Matcher::Matcher() : _parent(NULL), _reverse(false), _any(false), _group(false) {}
	Matcher::~Matcher() {}
	bool & Matcher::debug() { return Matcher::_debug; }
	Quantity & Matcher::quantity() { return _quantity; }
	bool & Matcher::reverse() { return _reverse; }
	bool & Matcher::any() { return _any; }
	Position & Matcher::position() { return _position; }
	string & Matcher::charset() { return _charset; }
	void Matcher::setParent(Matcher * parent) {
		this->_parent = parent;
	}
	Matcher * Matcher::getParent() {
		return _parent;
	}
	vector<AutoRef<Matcher> > & Matcher::elements() { return _elements; }
	vector<AutoRef<Matcher> > & Matcher::alters() { return _alters; }
	void Matcher::addChild(AutoRef<Matcher> child) {
		child->setParent(this);
		elements().push_back(child);
	}
	bool & Matcher::group() { return _group; }
	void Matcher::altering() {
		AutoRef<Matcher> alter(new Matcher);
		alter->elements() = _elements;
		_alters.push_back(alter);
		_elements.clear();
	}
	MatchResult Matcher::match(const string & text) {
		MatchResult result;
		LOGD(_debug, " @@ MATCH -- {{{" << toString() << "}}}");
		if (!_elements.empty()) {
			int i = 0;
			size_t len = 0;
			for (size_t s = len; s < text.size() && _quantity.testUnderLimit(i); i++) {
				bool br = false;
				for (vector<AutoRef<Matcher> >::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
					LOGD(_debug, " @@ 2-MATCHER -- {" << (*iter)->toString() << " => " << text.substr(s) << "}");
					MatchResult ret = (*iter)->match(text.substr(s));
					if (ret.matchType() == MatchType::UNMATCHED) {
						LOGD(_debug, "  >> UNMATCHED");
						br = true;
						break;
					}
					result.appendGroups(ret.groups());
					s += ret.length();
				}
				if (br) {
					break;
				}
				len += s;
			}
			if (_quantity.testInRange(i)) {
				result.matchType() = (len == text.size() ? MatchType::FULL_MATCHED : MatchType::PARTIAL_MATCHED);
				result.length() = len;
				if (group()) {
					result.appendGroup(text.substr(0, len));
				}
			}
		} else if (!_alters.empty()) {
			size_t len = 0;
			vector<string> groups;
			for (vector<AutoRef<Matcher> >::iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
				MatchResult ret = (*iter)->match(text);
				if (ret.length() > len) {
					groups = ret.groups();
					len = ret.length();
				}
			}
			result.appendGroups(groups);
			if (len > 0) {
				result.matchType() = (len == text.size() ? MatchType::FULL_MATCHED : MatchType::PARTIAL_MATCHED);
				result.length() = len;
				if (group()) {
					result.appendGroup(text.substr(0, len));
				}
			}
		} else if (!_charset.empty()) {
			int i = 0;
			for (; i < (int)text.size() && _quantity.testUnderLimit(i); i++) {
				LOGD(_debug, " @@ CHARSET -- '" << text[i] << "' IN '" << _charset << "'");
				if (ch_in(text[i], _charset) == (reverse() ? true:false)) {
					LOGD(_debug, "  >> NOT MEMBER");
					break;
				}
			}
			if (_quantity.testInRange(i)) {
				result.matchType() = (i == text.size() ? MatchType::FULL_MATCHED : MatchType::PARTIAL_MATCHED);
				result.length() = i;
				if (group()) {
					result.appendGroup(text.substr(0, i));
				}
			}
		} else if (_any) {
			int i = 0;
			AutoRef<Matcher> next;
			if (getParent()) {
				next = getParent()->nextMatcher(this);
			}
			LOGD(_debug, " @@ [ANY] NEXT MATCHER -- " << (next.nil() == true ? "(nil)" : next->toString()));
			for (; i < (int)text.size() && _quantity.testUnderLimit(i); i++) {
				LOGD(_debug, " @@ [ANY] TARGET -- " << text.substr(i));
				if (next.nil() == false && next->match(text.substr(i)).matchType() != MatchType::UNMATCHED) {
					LOGD(_debug, "  >> break - NEXT MATCHED");
					break;
				}
			}
			if (_quantity.testInRange(i)) {
				result.matchType() = (i == text.size() ? MatchType::FULL_MATCHED : MatchType::PARTIAL_MATCHED);
				result.length() = i;
				if (group()) {
					result.appendGroup(text.substr(0, i));
				}
			}
		}
		return result;
	}

	AutoRef<Matcher> Matcher::nextMatcher(Matcher * target) {
		for (vector<AutoRef<Matcher> >::iterator it = _elements.begin(); it != _elements.end(); it++) {
			if (&(*it) == target && (it + 1 != _elements.end())) {
				return *(it+1);
			}
		}
		if (getParent()) {
			return getParent()->nextMatcher(this);
		}
		return AutoRef<Matcher>();
	}
	
	string Matcher::toString() const {
		string ret = "";
		if (_group) {
			ret.append("|G|");
		}
		if (_reverse) {
			ret.append("|R|");
			ret.append("[" + _charset + "]" + _quantity.toString());
		} else if (_any) {
			ret.append("|*|" + _quantity.toString());
		} else if (_charset.size() == 1) {
			ret.append("\"" + _charset + "\"" + _quantity.toString());
		} else if (!_charset.empty()) {
			ret.append("[" + _charset + "]" + _quantity.toString());
		} else if (!_elements.empty()) {
			ret.append("(");
			for (vector<AutoRef<Matcher> >::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter != _elements.begin()) {
					ret.append(", ");
				}
				ret.append((*iter)->toString());
			}
			if (!_alters.empty()) {
				for (vector<AutoRef<Matcher> >::const_iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
					ret.append("|");
					ret.append((*iter)->toString());
				}
			} else {
				ret.append(")");
				ret.append(_quantity.toString());
			}
		} else if (!_alters.empty()) {
			ret.append("<");
			for (vector<AutoRef<Matcher> >::const_iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
				if (iter != _alters.begin()) {
					ret.append("|");
				}
				ret.append((*iter)->toString());
			}
			ret.append(">");
			ret.append(_quantity.toString());
		} else {
			ret.append("(nil)");
		}
		return ret;
	}

	/**
	 * @brief 
	 */
	bool numberp(const string & tok) {
		if (tok[0] == '-' && tok.find_first_not_of("0123456789", 1) == string::npos) {
			return true;
		} else if (tok.find_first_not_of("0123456789", 1) == string::npos) {
			return true;
		}
		return false;
	}

	int number(const string & tok) {
		return Text::toInt(tok);
	}

	Quantity makeQuantity(Iterator<string> & tokens_iter) {
		Quantity quantity;
		string tok = *tokens_iter;
		if (tok == "*") {
			quantity.initial() = 0;
			quantity.limit() = -1;
		} else if (tok == "+") {
			quantity.initial() = 1;
			quantity.limit() = -1;
		} else if (tok == "?") {
			quantity.initial() = 0;
			quantity.limit() = 1;
		} else if (tok == "{") {
			size_t i = 0;
			string s;
			string e;
			bool comma = false;
			while (tokens_iter.has() && tokens_iter.next() != "}") {
				string t = *tokens_iter;
				if (numberp(t)) {
					switch (i) {
					case 0:
						s = t;
						break;
					case 1:
						e = t;
						break;
					default:
						throw Exception("invalid quantification syntax - 'unexpected number occurrence'");
					}
					i++;
				} else if (t == ",") {
					comma = true;
				} else {
					throw Exception("invalid quntification syntax");
				}
			}
			if (s.empty()) {
				throw Exception("invalid quantification syntax");
			}
			if (*tokens_iter != "}") {
				throw Exception("invalid quantification syntax");
			}
			if (comma) {
				quantity.initial() = number(s);
				if (e.empty()) {
					quantity.limit() = -1;
				} else {
					quantity.limit() = number(e);
				}
			} else {
				quantity.initial() = number(s);
				quantity.limit() = number(s);
			}
		}
		return quantity;
	}


	/**
	 * @brief regex
	 */

	bool Regex::_debug = false;
	
	Regex::Regex() {
	}
	Regex::Regex(const string & regex) : _regex(regex) {
	}
	Regex::~Regex() {
	}
	bool & Regex::debug() {
		return _debug;
	}
	std::string & Regex::regex() {
		return _regex;
	}
	AutoRef<Matcher> Regex::makeMatcher() {
		return Regex::makeMatcher(_regex);
	}
	AutoRef<Matcher> Regex::makeMatcher(const string & regex) {
		vector<string> vec = tokenize(regex);
		Iterator<string> iter(vec);
		return makeMatcher(iter);
	}
	AutoRef<Matcher> Regex::makeMatcher(Iterator<string> & tokens_iter) {
		AutoRef<Matcher> root(new Matcher);
		makeMatcher_r(tokens_iter, root);
		return root;
	}
	void Regex::makeMatcher_r(Iterator<string> & tokens_iter, AutoRef<Matcher> parent) {
		
		// ?*+{}()[]|-^$.
		// ?*+{
	
		for (;tokens_iter.has(); tokens_iter++) {
		
			string token = *tokens_iter;

			if (contains(token, Text::toVector("?", "*", "+", "{", (const char *)NULL))) {
				Quantity quantity = makeQuantity(tokens_iter);
				if (parent->elements().size() == 0) {
					throw Exception("invalid syntax - 'no matcher exists'");
				}
				if ((*parent->elements().rbegin())->quantity().occured()) {
					throw Exception("invalid syntax - 'unexpected quantification'");
				}
				(*parent->elements().rbegin())->quantity() = quantity;
			} else if (token == "[") {
				AutoRef<Matcher> matcher(new Matcher);
				string charset;
				size_t i = 0;
				tokens_iter++;
				while (tokens_iter.has() && *tokens_iter != "]") {
					string t = tokens_iter.next();
					if (i == 0 && t == "^") {
						matcher->reverse() = true;
						t = tokens_iter.next();
					}
					i++;
					if (t == "-") {
						char to = (*tokens_iter)[0];
						char ch = *charset.rbegin();
						if (ch_in(ch, "0123456789")) {
							if (!ch_in(to, "0123456789")) {
								throw Exception("invalid syntax");
							}
							while (++ch <= to) {
								charset.append(1, ch);
							}
						} else if (ch_in(ch, "abcdefghijklmnopqrstuvwxyz")) {
							if (ch_in(to, "abcdefghijklmnopqrstuvwxyz")) {
								while (++ch <= 'z') {
									charset.append(1, ch);
								}
							} else if (ch_in(to, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
								ch = 'A';
								while (ch <= to) {
									charset.append(1, ch++);
								}
							} else {
								throw Exception("invliad syntax");
							}
						} else if (ch_in(ch, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
							if (!ch_in(to, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
								throw Exception("invliad syntax");
							}
							while (++ch <= to) {
								charset.append(1, ch);
							}
						}
					} else {
						charset.append(t);
					}
				}
				matcher->charset() = charset;
				parent->addChild(matcher);
			} else if (token == "(") {
				AutoRef<Matcher> elem(new Matcher);
				elem->group() = true;
				tokens_iter++;
				makeMatcher_r(tokens_iter, elem);
				parent->addChild(elem);
			} else if (token == ")") {
				break;
			} else if (token == "|") {
				if (parent->elements().empty()) {
					throw Exception("syntax error - empty altering");
				}
				parent->altering();
			} else if (token == ".") {
				AutoRef<Matcher> matcher(new Matcher);
				matcher->any() = true;
				parent->addChild(matcher);
			} else if (token == "^") {
				if (tokens_iter.idx() > 0) {
					AutoRef<Matcher> matcher(new Matcher);
					matcher->charset() = token;
					parent->addChild(matcher);
				} else {
					parent->position().start_of_line() = true;
				}
			} else if (token == "$") {
				if (tokens_iter.has_next()) {
					AutoRef<Matcher> matcher(new Matcher);
					matcher->charset() = token;
					parent->addChild(matcher);
				} else {
					parent->position().end_of_line() = true;
				}
			} else if (contains(token, Text::toVector("?", "*", "{", "}", "|", (const char*)NULL))){
				throw Exception("unexpected symbol");
			} else {
				AutoRef<Matcher> matcher(new Matcher);
				matcher->charset() = token;
				parent->addChild(matcher);
			}
		}
		if (!parent->alters().empty()) {
			if (parent->elements().empty()) {
				throw Exception("syntax error - empty altering");
			}
			parent->altering();
		}
	}
	vector<string> Regex::tokenize(const string & regex) {
		string special = "?*+{}()[]|-^$.";
		vector<string> tokens;
		for (size_t i = 0; i < regex.size(); i++) {
		
			if (regex[i] == '[') {
				for (; i < regex.size(); i++) {
					tokens.push_back(string(1, regex[i]));
					if (regex[i] == ']') {
						break;
					}
				}
			} else if (ch_in(regex[i], special)) {
				tokens.push_back(string(1, regex[i]));
			} else {

				if (regex[i] == '\\') {
					if (++i >= regex.size()) {
						throw Exception("unexpected end of string");
					}
					tokens.push_back("\\" + string(1, regex[i]));
				} else {
					tokens.push_back(string(1, regex[i]));
				}
			}
		}
		return tokens;
	}
}
