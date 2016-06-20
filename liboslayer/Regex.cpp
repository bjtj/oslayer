#include "Regex.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	
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
	 * @brief 
	 */
	Quantity::Quantity() : _occured(false), _initial(1), _limit(1) {}
	Quantity::~Quantity() {}
	bool & Quantity::occured() { return _occured; }
	int & Quantity::initial() { return _initial; }
	int & Quantity::limit() { return _limit; }
	string Quantity::toString() {
		return "{" + Text::toString(_initial) + ", " + Text::toString(_limit) + "}";
	}

	/**
	 * @brief 
	 */
	Position::Position() : _start_of_line(false), _end_of_line(false) {}
	Position::~Position() {}
	bool & Position::start_of_line() { return _start_of_line; }
	bool & Position::end_of_line() { return _end_of_line; }

	/**
	 * @brief 
	 */

	MatchResult::MatchResult() : _matched(false), _length(0) {}
	MatchResult::~MatchResult() {}
	bool & MatchResult::matched() { return _matched; }
	size_t & MatchResult::length() { return _length; }
	vector<string> & MatchResult::group() { return _group; }


	/**
	 * @brief 
	 */

	Matcher::Matcher() : _reverse(false), _any(false) {}
	Matcher::~Matcher() {}
	Quantity & Matcher::quantity() { return _quantity; }
	bool & Matcher::reverse() { return _reverse; }
	bool & Matcher::any() { return _any; }
	Position & Matcher::position() { return _position; }
	string & Matcher::charset() { return _charset; }
	vector<Matcher> & Matcher::elements() { return _elements; }
	vector<Matcher> & Matcher::alters() { return _alters; }
	void Matcher::altering() {
		Matcher alter;
		alter.elements() = _elements;
		_alters.push_back(alter);
		_elements.clear();
	}
	
	MatchResult Matcher::match(const string & text) {
		MatchResult result;
		if (!_elements.empty()) {
			int i = 0;
			size_t len = 0;
			for (size_t s = len; s < text.size() && (_quantity.limit() < 0 || i < _quantity.limit()); i++) {
				bool br = false;
				for (vector<Matcher>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
					MatchResult ret = iter->match(text.substr(s));
					if (!ret.matched()) {
						br = true;
						break;
					}
					s += ret.length();
				}
				if (br) {
					break;
				}
				len += s;
			}

			if (i >= _quantity.initial() && (_quantity.limit() < 0 || i <= _quantity.limit())) {
				result.matched() = true;
				result.length() = len;
			}
			
		} else if (!_alters.empty()) {
			size_t len = 0;
			for (vector<Matcher>::iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
				MatchResult ret = iter->match(text);
				if (ret.length() > len) {
					len = ret.length();
				}
			}
			if (len > 0) {
				result.matched() = true;
				result.length() = len;
			}
		} else if (!_charset.empty()) {
			int i = 0;
			for (; i < (int)text.size() && (_quantity.limit() < 0 || i < _quantity.limit()); i++) {
				if (!ch_in(text[i], _charset)) {
					break;
				}
			}
			if (i >= _quantity.initial() && (_quantity.limit() < 0 || i <= _quantity.limit())) {
				result.matched() = true;
				result.length() = i;
			}
		}
		return result;
	}
	string Matcher::toString() {
		if (_reverse) {
			return "*reverse*";
		}
		if (_any) {
			return "*any*" + _quantity.toString();
		}
		// if (!_plain.empty()) {
		if (_charset.size() == 1) {
			return "\"" + _charset + "\"" + _quantity.toString();
		}
		if (!_charset.empty()) {
			return "[" + _charset + "]" + _quantity.toString();
		}
		if (!_elements.empty()) {
			string ret = "(";
			for (vector<Matcher>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter != _elements.begin()) {
					ret.append(", ");
				}
				ret.append(iter->toString());
			}

			if (!_alters.empty()) {
				for (vector<Matcher>::iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
					ret.append("|");
					ret.append(iter->toString());
				}
				return ret;
			}
			
			ret.append(")");
			ret.append(_quantity.toString());
			return ret;
		}

		if (!_alters.empty()) {
			string ret = "<";
			for (vector<Matcher>::iterator iter = _alters.begin(); iter != _alters.end(); iter++) {
				if (iter != _alters.begin()) {
					ret.append("|");
				}
				ret.append(iter->toString());
			}
			ret.append(">");
			ret.append(_quantity.toString());
			return ret;
		}
		
		return "(nil)";
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
		string tok = tokens_iter.get();
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
			while (tokens_iter.hasNext() && tokens_iter.next() != "}") {
				string t = tokens_iter.get();
				if (numberp(t)) {
					switch (i) {
					case 0:
						s = t;
						break;
					case 1:
						e = t;
						break;
					default:
						throw "invalid quantification syntax - 'unexpected number occurrence'";
					}
					i++;
				} else if (t == ",") {
					comma = true;
				} else {
					throw "invalid quntification syntax";
				}
			}
			if (s.empty()) {
				throw "invalid quantification syntax";
			}
			if (tokens_iter.get() != "}") {
				throw "invalid quantification syntax";
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

	Regex::Regex() {
	}
	Regex::~Regex() {
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
						throw "unexpected end of string";
					}
					tokens.push_back("\\" + string(1, regex[i]));
				} else {
					tokens.push_back(string(1, regex[i]));
				}
			}
		}
		return tokens;
	}
	void Regex::makeMatcher(Iterator<string> & tokens_iter, Matcher & parent) {
		
		// ?*+{}()[]|-^$.
		// ?*+{
	
		for (;tokens_iter.hasNext(); tokens_iter++) {
		
			string token = tokens_iter.get();

			// cout << "#" << token << "#" << endl;

			if (contains(token, Text::toVector("?", "*", "+", "{", NULL))) {
				Quantity quantity = makeQuantity(tokens_iter);
				if (parent.elements().size() == 0) {
					throw "invalid syntax - 'no matcher exists'";
				}
				if ((*parent.elements().rbegin()).quantity().occured()) {
					throw "invalid syntax - 'unexpected quantification'";
				}
				(*parent.elements().rbegin()).quantity() = quantity;
			} else if (token == "[") {
				Matcher matcher;
				string charset;
				size_t i = 0;
				tokens_iter++;
				while (tokens_iter.hasNext() && tokens_iter.get() != "]") {
					string t = tokens_iter.next();
					if (i == 0 && t == "^") {
						matcher.reverse() = true;
					}
					i++;
					if (t == "-") {
						char to = tokens_iter.get()[0];
						char ch = *charset.rbegin();
						if (ch_in(ch, "0123456789")) {
							if (!ch_in(to, "0123456789")) {
								throw "invalid syntax";
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
								throw "invliad syntax";
							}
						} else if (ch_in(ch, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
							if (!ch_in(to, "ABCDEFGHIJKLMNOPQRSTUVWXYZ")) {
								throw "invliad syntax";
							}
							while (++ch <= to) {
								charset.append(1, ch);
							}
						}
					} else {
						charset.append(t);
					}
				}
				matcher.charset() = charset;
				parent.elements().push_back(matcher);
			} else if (token == "(") {
				Matcher elem;
				tokens_iter++;
				makeMatcher(tokens_iter, elem);
				parent.elements().push_back(elem);
			} else if (token == ")") {
				break;
			} else if (token == "|") {
				if (parent.elements().empty()) {
					throw "syntax error - empty altering";
				}
				parent.altering();
			} else if (token == ".") {
				Matcher matcher;
				matcher.any() = true;
				parent.elements().push_back(matcher);
			} else if (token == "^") {
				if (parent.elements().size() != 0) {
					throw "unexpected ^ occurence";
				}
				Matcher matcher;
				matcher.position().start_of_line() = true;
				parent.elements().push_back(matcher);
			} else if (token == "$") {
				if (tokens_iter.hasNext()) {
					throw "invalid syntax - $ must be last";
				}
				Matcher matcher;
				matcher.position().end_of_line() = true;
				parent.elements().push_back(matcher);
			} else if (contains(token, Text::toVector("?", "*", "{", "}", "|", NULL))){
				throw "unexpected symbol";
			} else {
				Matcher matcher;
				// matcher.plain() = token;
				matcher.charset() = token;
				parent.elements().push_back(matcher);
			}

			// cout << " $$$ " << parent.toString() << endl;
		}

		if (!parent.alters().empty()) {
			if (parent.elements().empty()) {
				throw "syntax error - empty altering";
			}
			parent.altering();
		}
	}
}
