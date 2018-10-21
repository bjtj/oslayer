#ifndef __BASE64_HPP__
#define __BASE64_HPP__

#include <string>

namespace osl {
	
    class Base64 {
    private:
    public:
	Base64();
	virtual ~Base64();

	static std::string encode(const std::string & plain);
	static std::string decode(const std::string & encoded);
    };
}

#endif
