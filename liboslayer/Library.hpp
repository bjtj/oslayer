#ifndef __LIBRARY_HPP__
#define __LIBRARY_HPP__

#include "os.hpp"

namespace osl {

#if defined(USE_UNIX_STD)

    typedef void * LIB_HANDLE;
    typedef void * SYM_HANDLE;

#elif defined(USE_MS_WIN)

    typedef HMODULE LIB_HANDLE;
    typedef FARPROC SYM_HANDLE;

#endif
	
    typedef void * (*func_arbitrary)();

    /**
     * @brief Library
     */
    class Library {
    public:
	class Symbol {
	private:
	    SYM_HANDLE handle;
	public:
	    Symbol(SYM_HANDLE handle);
	    virtual ~Symbol();
	    SYM_HANDLE getHandle();
	    short asShort();
	    unsigned short asUnsignedShort();
	    int asInt();
	    unsigned int asUnsignedInt();
	    long asLong();
	    unsigned long asUnsignedLong();
	    char asChar();
	    unsigned char asUnsignedChar();
	    char * asCharString();
	    func_arbitrary asFunc();
	    SYM_HANDLE & operator* ();
	};

    private:
	LIB_HANDLE _handle;
	std::string _path;
	std::string _name;
    public:
	Library(const std::string & name);
	Library(const std::string & path, const std::string & name);
	virtual ~Library();
	void load();
	void close();
	std::string & path();
	std::string & name();
	LIB_HANDLE & handle();
	Symbol symbol(const std::string & sym);
    };	
}

#endif
