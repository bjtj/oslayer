#ifndef __TEXT_HPP__
#define __TEXT_HPP__

#include "platform.hpp"

#include <string>
#include <vector>
#include <map>
#include <utility>
#include "StringElements.hpp"

namespace osl {

	class Text {
	private:
		Text();
		virtual ~Text();
		Text(const Text&);
		Text & operator=(const Text&);
	public:
		static bool isAlpha(char ch);
		static bool isAlphaNumeric(char ch);
		static bool isDigit(char ch);
		static bool isHexNumeric(char ch);
		static bool isLowercse(char ch);
		static bool isUppercase(char ch);
		static char upcase(char ch);
		static char downcase(char ch);
		static std::string upcase(const std::string & str);
		static std::string downcase(const std::string & str);
		static std::string capitalize(const std::string & str);
		static std::string trim(const std::string & str);
		static std::string ltrim(const std::string & str);
		static std::string rtrim(const std::string & str);
		static std::string trim(const std::string & str, const std::string & spaces);
		static std::string ltrim(const std::string & str, const std::string & spaces);
		static std::string rtrim(const std::string & str, const std::string & spaces);
		static bool match(const std::string & f, const std::string & s);
		static std::vector<std::string> split(const std::string & target, const std::string & sep);
		static std::string join(const std::vector<std::string> & vec, const std::string & glue);
		static bool contains(const std::vector<std::string> & vec, const std::string & target);
		static std::string replaceAll(const std::string & src, const std::string & match, const std::string & rep);
		static std::string quote(const std::string & str, const std::string & q = "'");
		static std::vector<std::string> toVector(const char * first, ...);
		static std::vector<std::string> toVector(int cnt, char ** strs);
		static std::map<std::string, std::string> toMap(const std::vector<std::string> & vec);
		static std::string toMapString(const std::map<std::string, std::string> & m,
									   const std::string & item_sep = ": ",
									   const std::string & line_sep = ", ");
		static std::string toMapString(const std::vector<std::pair<std::string, std::string> > & m,
									   const std::string & item_sep = ": ",
									   const std::string & line_sep = ", ");
		static int toInt(const std::string & str, int radix = 10);
		static long toLong(const std::string & str, int radix = 10);
		static float toFloat(const std::string & str);
		static double toDouble(const std::string & str);
        static std::string toString(short i);
        static std::string toString(int i);
        static std::string toString(long i);
        static std::string toString(long long i);
        static std::string toString(unsigned short i);
        static std::string toString(unsigned int i);
        static std::string toString(unsigned long i);
		static std::string toString(unsigned long long i);
		static std::string toString(float f);
		static std::string toString(double d);
        static std::string toHexString(short i);
        static std::string toHexString(int i);
		static std::string toHexString(long i);
        static std::string toHexString(long long i);
        static std::string toHexString(unsigned short i);
        static std::string toHexString(unsigned int i);
        static std::string toHexString(unsigned long i);
        static std::string toHexString(unsigned long long i);
		static std::string toUpperHexString(short i);
        static std::string toUpperHexString(int i);
		static std::string toUpperHexString(long i);
        static std::string toUpperHexString(long long i);
        static std::string toUpperHexString(unsigned short i);
        static std::string toUpperHexString(unsigned int i);
        static std::string toUpperHexString(unsigned long i);
        static std::string toUpperHexString(unsigned long long i);

		static std::string toString(const std::map<std::string, std::string> & m,
									const std::string & item_sep = ": ",
									const std::string & line_sep = ", ");
        static std::string toString(const std::vector<KeyValue> & lst,
									const std::string & item_sep = ": ",
									const std::string & line_sep = ", ");

		static bool startsWithIgnoreCase(const std::string & a, const std::string & b);
		static bool startsWith(const std::string & a, const std::string & b, bool ignorecase=false);
		static bool endsWithIgnoreCase(const std::string & a, const std::string & b);
		static bool endsWith(const std::string & a, const std::string & b, bool ignorecase=false);
		static int compareIgnoreCase(const std::string & a, const std::string & b);
		static bool equalsIgnoreCase(const std::string & a, const std::string & b);
        
        static std::string format(const char * fmt, ...);
		static std::string nformat(const size_t buf_size, const char * fmt, ...);
		static std::string toCommaNumber(const std::string & number);
	};
}

#endif
