#ifndef __ENVIRONMENT_HPP__
#define __ENVIRONMENT_HPP__

#include "os.hpp"
#include "Optional.hpp"
#include <map>
#include <string>

namespace osl {

    class Environment
    {
    private:
	Environment();
	virtual ~Environment();
    public:
	static Optional<std::string> get(const std::string & key);
	static void set(const std::string & key, const std::string & value);
	static void unset(const std::string & key);
	static std::map<std::string, std::string> all();
    };
}

#endif
