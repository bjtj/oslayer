#ifndef __URI_HPP__
#define __URI_HPP__

#include <string>

namespace UTIL {

	class Uri {
	private:
		std::string _scheme;
		std::string _username;
		std::string _password;
		std::string _hostname;
		int _port;
		std::string _path;
		std::string _query;
		std::string _fragment;
	public:
		Uri();
		virtual ~Uri();
		static Uri readFromString(const std::string & str);
		std::string & scheme();
		std::string & username();
		std::string & password();
		std::string & hostname();
		int & port();
		std::string & path();
		std::string & query();
		std::string & fragment();
		std::string scheme() const;
		std::string username() const;
		std::string password() const;
		std::string hostname() const;
		int port() const;
		std::string path() const;
		std::string query() const;
		std::string fragment() const;
	};


}

#endif
