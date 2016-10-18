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

	Library::Library(const string & name) : path("./"), name(name) {
		load(path, name);
	}
	Library::Library(const string & path, const string & name) : path(path), name(name) {
		load(path, name);
	}
	Library::~Library() {
		close();
	}
	void Library::load(const string & path, const string & name) {
		string fullpath = File::mergePaths(path, s_to_lib_name(name));
#if defined(USE_UNIX_STD)
		handle = dlopen(fullpath.c_str(), RTLD_LAZY);
		if (!handle) {
			throw Exception("dlopen() failed");
		}
		dlerror();
#elif defined(USE_MS_WIN)
		handle = LoadLibrary(fullpath.c_str());
		if (!handle) {
			throw Exception("LoadLibrary() failed");
		}
#else
		throw NotImplementedException("Not implemeneted - load library");
#endif
	}
	void Library::close() {
#if defined(USE_UNIX_STD)
		if (handle) {
			dlclose(handle);
			handle = NULL;
		}
#elif defined(USE_MS_WIN)
		if (handle) {
			FreeLibrary(handle);
			handle = NULL;
		}
#else
		throw NotImplementedException("Not implemeneted - free library");
#endif
	}
	string & Library::getPath() {
		return path;
	}
	string & Library::getName() {
		return name;
	}
	LIB_HANDLE Library::getHandle() {
		return handle;
	}
	Library::Symbol Library::getSymbol(const string & sym) {
#if defined(USE_UNIX_STD)
		SYM_HANDLE ret = dlsym(handle, sym.c_str());
		if (!ret) {
			throw Exception("dlsym() failed");
		}
		return Library::Symbol(ret);
#elif defined(USE_MS_WIN)
		SYM_HANDLE ret = GetProcAddress(handle, sym.c_str());
		if (!ret) {
			throw Exception("dlsym() failed");
		}
		return Library::Symbol(ret);
#else
		throw NotImplementedException("Not implemeneted - get symbol from library");
#endif
	}
	
}
