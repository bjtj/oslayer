#include "Library.hpp"
#include "File.hpp"

namespace OS {

	using namespace std;
	
	/**
	 * @brief Library
	 */

	Library::Symbol::Symbol(SYM_HANDLE handle) : handle(handle) {
	}
	Library::Symbol::~Symbol() {
	}
	SYM_HANDLE Library::Symbol::getHandle() {
		return handle;
	}
	short Library::Symbol::asShort() {
		return *(short*)handle;
	}
	unsigned short Library::Symbol::asUnsignedShort() {
		return *(unsigned short*)handle;
	}
	int Library::Symbol::asInt() {
		return *(int*)handle;
	}
	unsigned int Library::Symbol::asUnsignedInt() {
		return *(unsigned int*)handle;
	}
	long Library::Symbol::asLong() {
		return *(long*)handle;
	}
	unsigned long Library::Symbol::asUnsignedLong() {
		return *(unsigned long*)handle;
	}
	char Library::Symbol::asChar() {
		return *(char*)handle;
	}
	unsigned char Library::Symbol::asUnsignedChar() {
		return *(unsigned char*)handle;
	}
	char * Library::Symbol::asCharString() {
		return *((char**)handle);
	}
	func_arbitrary Library::Symbol::asFunc() {
		return (func_arbitrary)handle;
	}
	SYM_HANDLE & Library::Symbol::operator* () {
		return handle;
	}

	static string s_to_lib_name(const string & name) {
#if defined(USE_APPLE_STD)
		return "lib" + name + ".dylib";
#elif defined(USE_UNIX_STD)
		return "lib" + name + ".so";
#elif defined(USE_MS_WIN)
		return name + ".dll";
#else
		throw NotImplementedException("Not implemeneted - convert to native so name");
#endif
	}

	Library::Library(const string & name) : _path("./"), _name(name) {
	}
	Library::Library(const string & path, const string & name) : _path(path), _name(name) {
	}
	Library::~Library() {
		close();
	}
	void Library::load() {
		string fullpath = File::merge(_path, s_to_lib_name(_name));
#if defined(USE_UNIX_STD)
		_handle = dlopen(fullpath.c_str(), RTLD_LAZY);
		if (!_handle) {
			throw Exception("dlopen() failed");
		}
		dlerror();
#elif defined(USE_MS_WIN)
		_handle = LoadLibrary(fullpath.c_str());
		if (!_handle) {
			throw Exception("LoadLibrary() failed");
		}
#else
		throw NotImplementedException("Not implemeneted - load library");
#endif
	}
	void Library::close() {
#if defined(USE_UNIX_STD)
		if (_handle) {
			dlclose(_handle);
			_handle = NULL;
		}
#elif defined(USE_MS_WIN)
		if (_handle) {
			FreeLibrary(_handle);
			_handle = NULL;
		}
#else
		throw NotImplementedException("Not implemeneted - free library");
#endif
	}
	string & Library::path() {
		return _path;
	}
	string & Library::name() {
		return _name;
	}
	LIB_HANDLE & Library::handle() {
		return _handle;
	}
	Library::Symbol Library::symbol(const string & sym) {
#if defined(USE_UNIX_STD)
		SYM_HANDLE ret = dlsym(_handle, sym.c_str());
		if (!ret) {
			throw Exception("dlsym() failed");
		}
		return Library::Symbol(ret);
#elif defined(USE_MS_WIN)
		SYM_HANDLE ret = GetProcAddress(_handle, sym.c_str());
		if (!ret) {
			throw Exception("dlsym() failed");
		}
		return Library::Symbol(ret);
#else
		throw NotImplementedException("Not implemeneted - get symbol from library");
#endif
	}
	
}
