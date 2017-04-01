#ifndef __OS_HPP__
#define __OS_HPP__

/*
 * @brief standard header
 */
#include <string>
#include <fstream>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>
#include <cstdarg>
#include <stdint.h>

/**
 * @brief common feature
 * unused : http://stackoverflow.com/questions/3599160/unused-parameter-warnings-in-c-code
 */
#define SUPPRESS_UNUSED(x) (void)(x)

#include "platform.hpp"

/*
 * Semaphore
 */

#if defined(USE_APPLE_SEMAPHORE)

#	include <dispatch/dispatch.h>
typedef dispatch_semaphore_t SEM_HANDLE;

#elif defined(USE_POSIX_SEMAPHORE)

#	include <semaphore.h>
typedef sem_t SEM_HANDLE;

#elif defined(USE_WIN_SEMAPHORE)

#	define SEM_HANDLE HANDLE

#endif

/*
 * Thread
 */
#if defined(USE_PTHREAD)

#	include <pthread.h>

#if defined(USE_PRCTL)
#	include <sys/prctl.h>
#endif

typedef pthread_t THREAD_HANDLE;

#elif defined(USE_WIN_THREAD)

#	define WIN32_LEAN_AND_MEAN
#	define _WINSOCKAPI_ /* http://stackoverflow.com/questions/1372480/c-redefinition-header-files-winsock2-h */
#	include <Windows.h>
#	include <process.h>
typedef HANDLE THREAD_HANDLE;

#endif

/*
 * @brief Socket
 */
#if defined(USE_BSD_SOCKET) /* BSD */


#	include <arpa/inet.h>
#	include <sys/types.h>
#	include <sys/stat.h>
#	include <sys/socket.h>
#	include <ifaddrs.h>
#	include <netdb.h>
#	include <netinet/in.h>

#	if defined(USE_APPLE_STD)
#		include <net/if_dl.h>
#	else
#		include <linux/if_packet.h>
#		include <net/ethernet.h>
#	endif

typedef int SOCK_HANDLE;

#define INVALID_SOCKET -1

#elif defined(USE_WINSOCK2) /* winsock2 */

// reference: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms737629%28v=vs.85%29.aspx
// refenrece: https://msdn.microsoft.com/en-us/library/windows/desktop/aa365947%28v=vs.85%29.aspx

#	include <WinSock2.h>
#	include <WS2tcpip.h>
#	include <ws2ipdef.h>
#	include <iphlpapi.h>

#	pragma comment(lib, "Ws2_32.lib")
#	pragma comment(lib, "iphlpapi.lib")

/* Note: could also use malloc() and free() */
#	define MALLOC(x) HeapAlloc(GetProcessHeap(), 0, (x)) 
#	define FREE(x) HeapFree(GetProcessHeap(), 0, (x))

typedef SOCKET SOCK_HANDLE;

#endif

namespace OS {

#define DECL_EXCEPTION(NAME, BASE)										\
	class NAME : public BASE {											\
	public:																\
	explicit NAME() {/**/}												\
	explicit NAME(const std::string & message) : BASE(message) {/**/}	\
	explicit NAME(const char * message) : BASE(message) {/**/}			\
	explicit NAME(const std::string & message, int errorCode, int subErrorCode) \
		: BASE(message, errorCode, subErrorCode) {/**/}					\
	explicit NAME(const char * message, int errorCode, int subErrorCode) \
		: BASE(message, errorCode, subErrorCode) {/**/}					\
	virtual ~NAME() throw() {/**/}										\
	};

#define DECL_NAMED_EXCEPTION(NAME) DECL_EXCEPTION(NAME, OS::Exception)

	/**
	 * @brief Exception
	 */
	class Exception : public std::exception {
	private:
		std::string _message;
		int _error_code;
		int _sub_error_code;
	public:
		explicit Exception() : _error_code(-1), _sub_error_code(-1) {
		}
		explicit Exception(const std::string & message) :
			_message(message), _error_code(-1), _sub_error_code(-1) {
		}
		explicit Exception(const char * message) :
			_message(message), _error_code(-1), _sub_error_code(-1) {
		}
		explicit Exception(const std::string & message, int _error_code, int _sub_error_code) :
			_message(message), _error_code(_error_code), _sub_error_code(_sub_error_code) {
		}
		explicit Exception(const char * message, int _error_code, int _sub_error_code) :
			_message(message), _error_code(_error_code), _sub_error_code(_sub_error_code) {
		}
		virtual ~Exception() throw() {
		}
		int & error_code() {
			return _error_code;
		}
		int & sub_error_code() {
			return _sub_error_code;
		}
		std::string & message() {
			return _message;
		}
		virtual std::string toString() const {
			return _message;
		}
		virtual const char * what() const throw () {
			return _message.c_str();
		}
	};
    
    DECL_NAMED_EXCEPTION(NullException);
    DECL_NAMED_EXCEPTION(IOException);
    DECL_NAMED_EXCEPTION(NotImplementedException);
    DECL_NAMED_EXCEPTION(IllegalArgumentException);
	DECL_NAMED_EXCEPTION(BufferOverflowException);

	/**
	 * @brief no meaningful version string to distinguish
	 */
	std::string nomeaningfulVesion();

	/**
	 * @brief milli seconds sleep
	 */
	void idle(unsigned long timeout);

	/**
	 * @brief get tick count
	 */
	unsigned long tick_milli();
		
    
    /**
     * @brief system wide operation
     */
    class System {
    private:
        static System * systemImpl;
    protected:
        System();
        System(const System & other);
    public:
        virtual ~System();
        static System * getInstance();
		virtual void ignoreSigpipe() = 0;
    };

	/**
	 * @brief semaphore
	 */
	class Semaphore {
	private:
		int initial;
		mutable SEM_HANDLE handle;
	public:
		Semaphore(int initial);
		Semaphore(const Semaphore & other);
		virtual ~Semaphore();
		void wait() const;
		void post() const;
	};
    
    /**
     * @brief auto lock
     */
    class AutoLock {
    private:
        Semaphore & sem;
    public:
        AutoLock(Semaphore & sem);
        virtual ~AutoLock();
    };

	/*
	 * @brief Thread
	 */
	class Thread {
	private:
		
		THREAD_HANDLE handle;
		bool signal_interrupt;
		bool running;
		size_t stack_size;

	private:
		
		static unsigned int s_thread_id_seed;

	public:
		
		unsigned int id;

	private:

		void init();
		
	public:
		
		Thread();
		Thread(size_t stack_size);
		virtual ~Thread();

		unsigned int getId();

		void reset();

		bool start();
		void interrupt();
		bool interrupted();
		bool isRunning();

		virtual void onInterrupt();

		void wait();

		virtual void run() = 0;
	};
}

#endif
