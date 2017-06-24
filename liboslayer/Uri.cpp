#include "Uri.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;

	class Pair {
	private:
		string _first;
		string _second;
	public:
		Pair() {
		}
		virtual ~Pair() {
		}
		string & first() {
			return _first;
		}
		string & second() {
			return _second;
		}
	};

	static Pair read_pair(const string & str) {
		Pair pair;
		size_t f = str.find(":");
		if (f == string::npos) {
			pair.first() = str;
		} else {
			pair.first() = str.substr(0, f);
			pair.second() = str.substr(f+1);
		}
		return pair;
	}

	Uri::Uri() : _port(0) {
	}
	Uri::~Uri() {
	}
	Uri Uri::readFromString(const string & str) {
		Uri uri;
		if (str.empty()) {
			return uri;
		}
		size_t f = str.find(":");
		if (f != string::npos) {
			uri.scheme() = str.substr(0, f);
			f++;
		}
		if(str.find("//", f) != string::npos) {
			f += 2;
		}
		string auth;
		string path;
		size_t e = str.find("/", f);
		if (e == string::npos) {
			auth = str.substr(f);
		} else {
			auth = str.substr(f, e - (f));
			path = str.substr(e);
		}
		f = auth.find("@");
		if (f == string::npos) {
			Pair pair = read_pair(auth.substr(f+1));
			uri.hostname() = pair.first();
			uri.port() = Text::toInt(pair.second());
		} else {
			Pair pair = read_pair(auth.substr(0, f));
			uri.username() = pair.first();
			uri.password() = pair.second();
			pair = read_pair(auth.substr(f+1));
			uri.hostname() = pair.first();
			uri.port() = Text::toInt(pair.second());
		}

		f = path.find("?");
		if (f == string::npos) {
			size_t x = path.find("#");
			if (x == string::npos) {
				uri.path() = path;
			} else {
				uri.path() = path.substr(0, x);
				uri.fragment() = path.substr(x+1);
			}
		} else {
			size_t x = path.find("#", f+1);
			if (x == string::npos) {
				uri.path() = path.substr(0, f);
				uri.query() = path.substr(f+1);
			} else {
				uri.path() = path.substr(0, f);
				uri.query() = path.substr(f+1, x - (f+1));
				uri.fragment() = path.substr(x+1);
			}
		}
		
		return uri;
	}
	string & Uri::scheme() {
		return _scheme;
	}
	string & Uri::username() {
		return _username;
	}
	string & Uri::password() {
		return _password;
	}
	string & Uri::hostname() {
		return _hostname;
	}
	int & Uri::port() {
		return _port;
	}
	string & Uri::path() {
		return _path;
	}
	string & Uri::query() {
		return _query;
	}
	string & Uri::fragment() {
		return _fragment;
	}
	string Uri::scheme() const {
		return _scheme;
	}
	string Uri::username() const {
		return _username;
	}
	string Uri::password() const {
		return _password;
	}
	string Uri::hostname() const {
		return _hostname;
	}
	int Uri::port() const {
		return _port;
	}
	string Uri::path() const {
		return _path;
	}
	string Uri::query() const {
		return _query;
	}
	string Uri::fragment() const {
		return _fragment;
	}
}
