#ifndef __TEXT_HPP__
#define __TEXT_HPP__

#if defined(__APPLE__) || defined(__MACH__) /* mac os x */

#    define PLATFORM_APPLE

#	include <unistd.h>

#elif defined(unix) || defined(__unix__) || defined(__unix) /* unix or linux */

#    define PLATFORM_NIX

#	include <unistd.h>

#elif defined(_WIN32) || defined(_WIN64) /* windows */

#    define PLATFORM_WIN

#	define strcasecmp(x,y) _stricmp((x),(y))
#	define strncasecmp(x,y,z) _strnicmp((x),(y),(z))
#	define snprintf _snprintf_s

#endif

#include <string>
#include <vector>
#include <map>
#include <utility>
#include "StringElement.hpp"

namespace UTIL {

	class Text {
	private:
		Text();
		virtual ~Text();
		Text(const Text&);
		Text & operator=(const Text&);
	public:
		static std::string trim(std::string str);
		static bool match(std::string f, std::string s);
		static std::vector<std::string> split(std::string target, std::string sep);
		static std::string join(const std::vector<std::string> & vec, const std::string & glue);
		static bool contains(std::vector<std::string> & vec, std::string target);
		static std::string replaceAll(std::string src, std::string match, std::string rep);
		static std::string quote(std::string str, std::string q = "'");
		static std::string toMapString(std::map<std::string, std::string> & m,
									   std::string item_sep = ": ",
									   std::string line_sep = ", ");
		static std::string toMapString(std::vector<std::pair<std::string, std::string> > & m,
									   std::string item_sep = ": ",
									   std::string line_sep = ", ");
		static int toInt(std::string str, int radix = 10);
        static std::string toString(short i);
        static std::string toString(int i);
        static std::string toString(long i);
        static std::string toString(long long i);
        static std::string toString(unsigned short i);
        static std::string toString(unsigned int i);
        static std::string toString(unsigned long i);
		static std::string toString(unsigned long long i);
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
        
        static std::string toString(const NameValueList & lst, const std::string & item_sep = ": ", const std::string & line_sep = ", ");
        
		static bool startsWith(std::string a, std::string b, bool ignorecase=false);
		static bool endsWith(std::string a, std::string b, bool ignorecase=false);
		static int compareIgnoreCase(std::string a, std::string b);
		static bool equalsIgnoreCase(std::string a, std::string b);
	};
}

#endif
