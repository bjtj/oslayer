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

#define DECL_NAMED_ONLY_EXCEPTION(NAME) \
	class NAME : public OS::Exception { \
	public:										   \
		explicit NAME() {/**/}												\
		explicit NAME(const std::string & message) : OS::Exception(message) {/**/} \
		explicit NAME(const char * message) : OS::Exception(message) {/**/} \
		explicit NAME(const std::string & message, int errorCode, int subErrorCode) \
		: OS::Exception(message, errorCode, subErrorCode) {/**/} \
		explicit NAME(const char * message, int errorCode, int subErrorCode) \
		: OS::Exception(message, errorCode, subErrorCode) {/**/} \
		virtual ~NAME() throw() {/**/} \
	};

	/**
	 * @brief Exception
	 */
	class Exception : public std::exception {
	private:
		std::string message;
		int errorCode;
		int subErrorCode;
	public:
		explicit Exception() : errorCode(-1), subErrorCode(-1) {
		}
		explicit Exception(const std::string & message) :
			message(message), errorCode(-1), subErrorCode(-1) {
		}
		explicit Exception(const char * message) :
			message(message), errorCode(-1), subErrorCode(-1) {
		}
		explicit Exception(const std::string & message, int errorCode, int subErrorCode) :
			message(message), errorCode(errorCode), subErrorCode(subErrorCode) {
		}
		explicit Exception(const char * message, int errorCode, int subErrorCode) :
			message(message), errorCode(errorCode), subErrorCode(subErrorCode) {
		}
		virtual ~Exception() throw() {
		}
		int getErrorCode() {
			return errorCode;
		}
		void setErrorCode(int errorCode) {
			this->errorCode = errorCode;
		}
		int getSubErrorCode() {
			return subErrorCode;
		}
		void setSubErrorCode(int subErrorCode) {
			this->subErrorCode = subErrorCode;
		}
		std::string & getMessage() {
			return message;
		}
		void setMessage(const std::string & message) {
			this->message = message;
		}
		void setMessage(const char * message) {
			this->message = message;
		}
		virtual std::string toString() {
			return message;
		}
		virtual const char * what() const throw () {
			return message.c_str();
		}
	};
    
    DECL_NAMED_ONLY_EXCEPTION(NullException);
    DECL_NAMED_ONLY_EXCEPTION(IOException);
    DECL_NAMED_ONLY_EXCEPTION(NotImplementedException);
    DECL_NAMED_ONLY_EXCEPTION(IllegalArgumentException);
	DECL_NAMED_ONLY_EXCEPTION(BufferOverflowException);

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
    
    /**
     * @brief selection
     */
    class Selection {
        
    private:
        int fd;
        bool readable;
        bool writeable;
		bool except;
        
    public:
        Selection(int fd, bool readable, bool writeable, bool except);
        virtual ~Selection();
        
        int getFd();
        bool isReadable();
        bool isWritable();
		bool isExcept();
    };
    
    class Selector;
    
    /**
     * @brief Selectable interface
     */
    class Selectable {
    private:
    public:
		Selectable();
        virtual ~Selectable();
        virtual int getFd() = 0;
		virtual void registerSelector(Selector & selector, unsigned char flags);
		virtual void unregisterSelector(Selector & selector, unsigned char flags);
		virtual bool isReadable(Selector & selector);
		virtual bool isWritable(Selector & selector);
		virtual bool isExcept(Selector & selector);
    };

	/**
	 * @brief selector
	 */
	class Selector {
	public:
		const static unsigned char READ = 0x01;
		const static unsigned char WRITE = 0x02;
		const static unsigned char EXCEPT = 0x04;
		const static unsigned char FULL = 0x07;
		
	private:
		int maxfds;
        fd_set readfds;
        fd_set writefds;
		fd_set exceptfds;
		fd_set curreadfds;
        fd_set curwritefds;
		fd_set curexceptfds;
		std::vector<Selection> selections;
        
	public:
		Selector();
		virtual ~Selector();

		virtual void set(int fd, unsigned char flags);
		virtual void unset(int fd, unsigned char flags);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<Selection> & getSelections();
        virtual bool isRegistered(int fd, unsigned char type);
		virtual bool isReadable(int fd);
		virtual bool isReadable(Selectable & selectable);
		virtual bool isWritable(int fd);
		virtual bool isWritable(Selectable & selectable);
		virtual bool isExcept(int fd);
		virtual bool isExcept(Selectable & selectable);
	};


	/**
	 * @brief 
	 */
	class SharedSelector : public Selector {
	private:
		Semaphore semSet;
		Semaphore semCur;
	public:
		SharedSelector();
		virtual ~SharedSelector();

		virtual void set(int fd, unsigned char flags);
		virtual void unset(int fd, unsigned char flags);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<Selection> & getSelections();
		virtual bool isReadable(int fd);
		virtual bool isReadable(Selectable & selectable);
		virtual bool isWritable(int fd);
		virtual bool isWritable(Selectable & selectable);
		virtual bool isExcept(int fd);
		virtual bool isExcept(Selectable & selectable);
	};
}

#endif
