#include "Text.hpp"

#include <algorithm>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <cstdarg>

/**
 * @namespace UTIL
 */
namespace UTIL {

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

	static string spaces = " \t\r\n";

	/**
	 * @breif trim
	 */
	string Text::trim(const string & str) {
		
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

	string Text::ltrim(const string & str) {

		if (str.empty()) {
			return "";
		}

		size_t f = str.find_first_not_of(spaces);
		if (f == string::npos) {
			return "";
		}

		return str.substr(f);

	}
	string Text::rtrim(const string & str) {
		if (str.empty()) {
			return "";
		}

		size_t f = str.find_last_not_of(spaces);
		if (f == string::npos) {
			return "";
		}

		return str.substr(0, f);
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
		size_t s = 0;
		size_t f = 0;

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
	bool Text::contains(vector<string> & vec, string target) {
		return (std::find(vec.begin(), vec.end(), target) != vec.end());
	}

	/**
	 * @brief replace all
	 */
	string Text::replaceAll(string src, string match, string rep) {
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
		for (const char * str = va_arg(args, const char *); str; str = va_arg(args, const char *)) {
			vec.push_back(str);
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
	string Text::toMapString(map<string, string> & m, string item_sep, string line_sep) {
		
		string ret = "";

		for (map<string, string>::iterator it = m.begin(); it != m.end(); it++) {
			string name = it->first;
			string & value = it->second;

			if (!ret.empty()) {
				ret += line_sep;
			}

			ret += (name + item_sep + value);
		}

		return ret;
	}

	/**
	 * @brief to map string
	 */
	string Text::toMapString(vector<pair<string, string> > & m, string item_sep, string line_sep) {
		
		string ret = "";

		for (size_t i = 0; i < m.size(); i++) {
			string name = m[i].first;
			string & value = m[i].second;

			if (!ret.empty()) {
				ret += line_sep;
			}

			ret += (name + item_sep + value);
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
	string Text::toString(float f) {
		char num[512] = {0,};
		snprintf(num, sizeof(num), "%f", f);
		return string(num);
	}
    
    string Text::toString(const NameValueList & lst, const string & item_sep, const string & line_sep) {
        string ret;
        bool first = true;
        for (NameValueList::const_iterator iter = lst.begin(); iter != lst.end(); iter++) {
            
            if (first) {
                first = false;
            } else {
                ret.append(line_sep);
            }
            
            const NameValue & nv = *iter;
            ret.append(nv.const_name());
            ret.append(item_sep);
            ret.append(nv.const_value());
        }
        return ret;
    }

	/**
	 * @brief starts with
	 */
	bool Text::startsWith(string a, string b, bool ignorecase) {
		
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
	bool Text::endsWith(string a, string b, bool ignorecase) {

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
	int Text::compareIgnoreCase(string a, string b) {
		return strcasecmp(a.c_str(), b.c_str());
	}

	/**
	 * @brief equals ignore case
	 */
	bool Text::equalsIgnoreCase(string a, string b) {

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
        return buffer;
    }
}
