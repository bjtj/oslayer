#include "Text.hpp"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>
#include "os.hpp"

/**
 * @namespace osl
 */
namespace osl {

    using namespace std;

    /**
     * @brief constructor
     */
    Text::Text() {
    }
	
    /**
     * @brief destructor
     */
    Text::~Text() {
    }

    static string _alpha_lower("abcdefghijklmnopqrstuvwxyz");
    static string _alpha_upper("ABCDEFGHIJKLMNOPQRSTUVWXYZ");
    static string _digit("0123456789");
    static string _hex_numeric("0123456789abcdefABCDEF");
    static string _alpha = _alpha_lower + _alpha_upper;
    static string _alpha_numeric = _alpha + _digit;
    static string _spaces(" \t\r\n");

    bool Text::isAlpha(char ch) {
	return (_alpha.find(ch) != string::npos);
    }
	
    bool Text::isAlphaNumeric(char ch) {
	return (_alpha_numeric.find(ch) != string::npos);
    }
	
    bool Text::isDigit(char ch) {
	return (_digit.find(ch) != string::npos);
    }
	
    bool Text::isHexNumeric(char ch) {
	return (_hex_numeric.find(ch) != string::npos);
    }
	
    bool Text::isLowercse(char ch) {
	return (_alpha_lower.find(ch) != string::npos);
    }
	
    bool Text::isUppercase(char ch) {
	return (_alpha_upper.find(ch) != string::npos);
    }

    char Text::upcase(char ch) {
	size_t idx = _alpha_lower.find(ch);
	if (idx != string::npos) {
	    return _alpha_upper[idx];
	}
	return ch;
    }
    char Text::downcase(char ch) {
	size_t idx = _alpha_upper.find(ch);
	if (idx != string::npos) {
	    return _alpha_lower[idx];
	}
	return ch;
    }

    string Text::upcase(const string & str) {
	string ret;
	for (string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
	    ret.append(1, upcase(*iter));
	}
	return ret;
    }
	
    string Text::downcase(const string & str) {
	string ret;
	for (string::const_iterator iter = str.begin(); iter != str.end(); iter++) {
	    ret.append(1, downcase(*iter));
	}
	return ret;
    }
	
    string Text::capitalize(const string & str) {
	string ret;
	size_t idx = str.find_first_not_of(_spaces);
	if (idx == string::npos) {
	    return str;
	}
	if (idx > 0) {
	    ret.append(str.substr(0, idx));
	}
	ret.append(1, upcase(str[idx]));
	ret.append(str.substr(idx + 1));
	return ret;
    }

    /**
     * @breif trim
     */
    string Text::trim(const string & str) {
	return trim(str, _spaces);
    }
    string Text::ltrim(const string & str) {
	return ltrim(str, _spaces);
    }
    string Text::rtrim(const string & str) {
	return rtrim(str, _spaces);
    }
    string Text::trim(const string & str, const string & spaces) {
	size_t f = 0;
	size_t e = 0;
	if (str.empty()) {
	    return "";
	}
	f = str.find_first_not_of(spaces);
	if (f == string::npos) {
	    return "";
	}
	e = str.find_last_not_of(spaces);
	if (e == string::npos) {
	    return str.substr(f);
	}
	return str.substr(f, e - f + 1);
    }

    string Text::ltrim(const string & str, const string & spaces) {
	if (str.empty()) {
	    return "";
	}
	size_t f = str.find_first_not_of(spaces);
	if (f == string::npos) {
	    return "";
	}
	return str.substr(f);
    }
    string Text::rtrim(const string & str, const string & spaces) {
	if (str.empty()) {
	    return "";
	}
	for (size_t i = 0; i < str.size(); i++) {
	    if (spaces.find(str[str.size() - i - 1]) == string::npos) {
		return str.substr(0, str.size() - i);
	    }
	}
	return "";
    }

    /**
     * @brief match f can contains * for any 1..n, ? for any 1
     */
    bool Text::match(const string & f, const string & s) {

	if (f.empty() && !s.empty()) {
	    return false;
	}
		
	const char * fs = f.c_str();
	const char * fe = fs + f.length();
	const char * ss = s.c_str();
	const char * se = ss + s.length();

	int flip = 0;

	int i = 0;

	int just_cnt_match = 1;
	int any = 0;
	for (i = 0; *(fs + i); i++) {
	    if (*(fs + i) != '?' && *(fs + i) != '*') {
		just_cnt_match = 0;
		break;
	    }
	    if (*(fs+i) == '?') {
		any++;
	    }
	}

	if (just_cnt_match) {
	    return (s.length() >= (size_t)any) ? true : false;
	}
	
	// real checking

	while (1) {

	    if (fs == fe && ss == se) {
		return true;
	    }

	    if (!flip) {

		if (*fs == '*' && *(fs + 1) == *fe && *(ss + 1) == *se) {
		    return true;
		}
			
		if (*fs == '*') {

		    for (i = 1; *(fs + i) == '?'; i++) {
		    }

		    if (*(fs + i) == *(ss + (i - 1))) {
			fs += i;
			ss += (i-1);
			flip = !flip;
		    } else {
			ss++;
		    }
				
		    continue;
		}

		if (*fs == '?' || *fs == *ss) {
		    fs++;
		    ss++;
		    continue;
		}

		break;
			
	    } else {

		if (*fe == '*' && *(fe - 1) == *fs && *(se - 1) == *ss) {
		    return true;
		}

		if (*fe == '*') {

		    for (i = 1; *(fe - i) == '?'; i++) {
		    }

		    if (*(fe - i) == *(se - (i - 1))) {
			fe -= i;
			se -= (i - 1);
			flip = !flip;
		    } else {
			se--;
		    }
		    continue;
		}

		if (*fe == '?' || *fe == *se) {
		    fe--;
		    se--;
		    continue;
		}

		break;
	    }
	}

	return false;
    }

    static bool s_in(char ch, const string & toks) {
	for (string::const_iterator iter = toks.begin(); iter != toks.end(); iter++) {
	    if (ch == *iter) {
		return true;
	    }
	}
	return false;
    }

    /**
     * @brief split string with sep
     */
    vector<string> Text::split(const string & target, const string & sep) {

	vector<string> vec;

	if (target.empty()) {
	    return vec;
	}

	string buf;
	for (string::const_iterator iter = target.begin(); iter != target.end(); iter++) {
	    if (s_in(*iter, sep)) {
		if (buf.size() > 0) {
		    vec.push_back(buf);
		    buf.clear();
		}
	    } else {
		buf.append(1, *iter);
	    }
	}
	if (buf.size() > 0) {
	    vec.push_back(buf);
	}

	return vec;
    }

    /**
     * @brief join vector items
     */
    string Text::join(const vector<string> & vec, const string & glue) {

	string ret;
		
	for (size_t i = 0; i < vec.size(); i++) {
	    if (i > 0) {
		ret += glue;
	    }

	    ret += vec[i];
	}

	return ret;
    }

    /**
     * @brief check contains
     */
    bool Text::contains(const vector<string> & vec, const string & target) {
	for (vector<string>::const_iterator iter = vec.begin(); iter != vec.end(); iter++) {
	    if (*iter == target) {
		return true;
	    }
	}
	return false;
    }

    /**
     * @brief replace all
     */
    string Text::replaceAll(const string & src, const string & match, const string & rep) {
	string ret = src;
	size_t f = 0;
	while ((f = ret.find(match, f)) != string::npos) {
	    ret.replace(f, match.length(), rep);
	    f += rep.length();
	}
	return ret;
    }

    /**
     * @brief quote
     */
    string Text::quote(const string & str, const string & q) {
	return q + str + q;
    }
	
    vector<string> Text::toVector(const char * first, ...) {
	vector<string> vec;
	vec.push_back(first);
	va_list args;
        va_start(args, first);
	const char * str = NULL;
	while ((str = (const char*)va_arg(args, const char *)) != NULL) {
	    vec.push_back(string(str));
	}
        va_end(args);
	return vec;
    }
	
    vector<string> Text::toVector(int cnt, char ** strs) {
	vector<string> vec;
	for (int i = 0; i < cnt; i++) {
	    if (strs[i]) {
		vec.push_back(string(strs[i]));
	    } else {
		vec.push_back("(null)");
	    }
	}
	return vec;
    }

    map<string, string> Text::toMap(const vector<string> & vec) {
	map<string, string> m;
	for (size_t i = 0; i < vec.size(); i += 2) {
	    string n = vec[i];
	    string v = ((i + 1 < vec.size()) ? vec[i+1] : "");
	    m[n] = v;
	}
	return m;
    }

    /**
     * @brief to map string
     */
    string Text::toMapString(const map<string, string> & m, const string & item_sep, const string & line_sep) {
	string ret = "";
	for (map<string, string>::const_iterator it = m.begin(); it != m.end(); it++) {
	    if (!ret.empty()) {
		ret += line_sep;
	    }
	    ret += (it->first + item_sep + it->second);
	}
	return ret;
    }

    /**
     * @brief to map string
     */
    string Text::toMapString(const vector<pair<string, string> > & m, const string & item_sep, const string & line_sep) {
	string ret = "";
	for (size_t i = 0; i < m.size(); i++) {
	    if (!ret.empty()) {
		ret += line_sep;
	    }
	    ret += (m[i].first + item_sep + m[i].second);
	}
	return ret;
    }

    /**
     * @brief to int
     */
    int Text::toInt(const string & str, int radix) {
	return (int)strtol(str.c_str(), NULL, radix);
    }

    long Text::toLong(const string & str, int radix) {
	return (long)strtol(str.c_str(), NULL, radix);
    }

    float Text::toFloat(const string & str) {
	return (float)atof(str.c_str());
    }
	
    double toDouble(const string & str) {
	return (double)atof(str.c_str());
    }

    /**
     * @brief to string
     */
    
    string Text::toString(short i) {
        return toString((long long)i);
    }
    string Text::toString(int i) {
        return toString((long long)i);
    }
    string Text::toString(long i) {
        return toString((long long)i);
    }
    string Text::toString(long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%lld", i);
        return string(num);
    }
    string Text::toString(unsigned short i) {
        return toString((unsigned long long)i);
    }
    string Text::toString(unsigned int i) {
        return toString((unsigned long long)i);
    }
    string Text::toString(unsigned long i) {
        return toString((unsigned long long)i);
    }
    string Text::toString(unsigned long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%llu", i);
        return string(num);
    }
    string Text::toString(float f) {
	char num[512] = {0,};
        snprintf(num, sizeof(num), "%f", f);
        return string(num);
    }
    string Text::toString(double d) {
	char num[512] = {0,};
        snprintf(num, sizeof(num), "%lf", d);
        return string(num);
    }
    string Text::toHexString(short i) {
        return toHexString((long long)i);
    }
    string Text::toHexString(int i) {
        return toHexString((long long)i);
    }
    string Text::toHexString(long i) {
        return toHexString((long long)i);
    }
    string Text::toHexString(long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%llx", i);
        return string(num);
    }
    string Text::toHexString(unsigned short i) {
        return toHexString((unsigned long long)i);
    }
    string Text::toHexString(unsigned int i) {
        return toHexString((unsigned long long)i);
    }
    string Text::toHexString(unsigned long i) {
        return toHexString((unsigned long long)i);
    }
    string Text::toHexString(unsigned long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%llx", i);
        return string(num);
    }

    string Text::toUpperHexString(short i) {
        return toUpperHexString((long long)i);
    }
    string Text::toUpperHexString(int i) {
        return toUpperHexString((long long)i);
    }
    string Text::toUpperHexString(long i) {
        return toUpperHexString((long long)i);
    }
    string Text::toUpperHexString(long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%llX", i);
        return string(num);
    }
    string Text::toUpperHexString(unsigned short i) {
        return toUpperHexString((unsigned long long)i);
    }
    string Text::toUpperHexString(unsigned int i) {
        return toUpperHexString((unsigned long long)i);
    }
    string Text::toUpperHexString(unsigned long i) {
        return toUpperHexString((unsigned long long)i);
    }
    string Text::toUpperHexString(unsigned long long i) {
        char num[512] = {0,};
        snprintf(num, sizeof(num), "%llX", i);
        return string(num);
    }
    string Text::toString(const map<string, string> & m, const string & item_sep, const string & line_sep) {
	string ret;
        bool first = true;
        for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
            if (first) {
                first = false;
            } else {
                ret.append(line_sep);
            }
            ret.append(iter->first);
            ret.append(item_sep);
            ret.append(iter->second);
        }
        return ret;
    }
    string Text::toString(const vector<KeyValue> & lst, const string & item_sep, const string & line_sep) {
        string ret;
        bool first = true;
        for (vector<KeyValue>::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
            if (first) {
                first = false;
            } else {
                ret.append(line_sep);
            }
            const KeyValue & kv = *iter;
            ret.append(kv.key());
            ret.append(item_sep);
            ret.append(kv.value());
        }
        return ret;
    }

    /**
     * @brief starts with
     */

    bool Text::startsWithIgnoreCase(const string & a, const string & b) {
	return startsWith(a, b, true);
    }
	
    bool Text::startsWith(const string & a, const string & b, bool ignorecase) {
		
	if (b.empty()) {
	    return false;
	}

	if (a.length() >= b.length()) {

	    if (ignorecase) {
		return !(strcasecmp(a.substr(0, b.length()).c_str(), b.c_str()));
	    }
			
	    return !(a.substr(0, b.length()).compare(b));
	}

	return false;
    }

    /**
     * @brief ends with
     */

    bool Text::endsWithIgnoreCase(const string & a, const string & b) {
	return endsWith(a, b, true);
    }

    bool Text::endsWith(const string & a, const string & b, bool ignorecase) {
	if (b.empty()) {
	    return false;
	}
	if (a.length() >= b.length()) {
	    if (ignorecase) {
		return !(strcasecmp(a.substr(a.length() - b.length()).c_str(), b.c_str()));
	    }
	    return !(a.substr(a.length() - b.length()).compare(b));
	}
	return false;
		
    }

    /**
     * @brief compare ignore case
     */
    int Text::compareIgnoreCase(const string & a, const string & b) {
	return strcasecmp(a.c_str(), b.c_str());
    }

    /**
     * @brief equals ignore case
     */
    bool Text::equalsIgnoreCase(const string & a, const string & b) {
	if (a.length() != b.length()) {
	    return false;
	}
	return (!strcasecmp(a.c_str(), b.c_str()) ? true : false);
    }
    
    /**
     * @brief format
     */
    string Text::format(const char * fmt, ...) {
        char buffer[1024] = {0,};
        va_list args;
        va_start(args, fmt);
        osl_vsnprintf(buffer, sizeof(buffer), fmt, args);
        va_end(args);
        return string(buffer);
    }

    /**
     * @brief n-format
     */
    string Text::nformat(const size_t buf_size, const char * fmt, ...) {
	char * buffer = new char[buf_size];
	memset(buffer, 0, buf_size);
        va_list args;
        va_start(args, fmt);
        osl_vsnprintf(buffer, buf_size, fmt, args);
        va_end(args);
	string ret(buffer);
	delete[] buffer;
        return ret;
    }

    /**
     * to comma number
     */
    string Text::toCommaNumber(const string & number) {
	if (number.empty()) {
	    return number;
	}
	char sign = '\0';
	string integer;
	string decimal;
	if (number[0] == '-' || number[0] == '+') {
	    sign = number[0];
	}
	size_t f = number.find(".");
	if (f == string::npos) {
	    integer = number.substr((sign ? 1 : 0));
	} else {
	    integer = number.substr((sign ? 1 : 0), f - (sign ? 1 : 0));
	    decimal = number.substr(f + 1);
	}
	string iret;
	for (size_t i = 0; i < integer.size(); i++) {
	    char ch = integer[integer.size() - i - 1];
	    if (!isdigit(ch)) {
		throw Exception("wrong number format");
	    }
	    if (i != 0 && i % 3 == 0) {
		iret.insert(0, 1, ',');
	    }
	    iret.insert(0, 1, ch);
	}
	string dret;
	for (size_t i = 0; i < decimal.size(); i++) {
	    char ch = decimal[i];
	    if (!isdigit(ch)) {
		throw Exception("wrong number format");
	    }
	    if (i != 0 && i % 3 == 0) {
		dret.append(1, ',');
	    }
	    dret.append(1, ch);
	}
	string ret;
	if (sign) {
	    ret.append(1, sign);
	}
	ret.append(iret);
	if (dret.empty() == false) {
	    ret.append(".");
	    ret.append(dret);
	}
	return ret;
    }
}
