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

		void join();

		virtual void run() = 0;
	};
    
    /**
     * @brief InetAddress
     */
    class InetAddress {
    public:

		/**
		 * @brief InetVersion
		 */
        class InetVersion {
        public:
            static const int UNKNOWN = 0;
            static const int INET4 = 1;
            static const int INET6 = 2;
        private:
            int version;
        public:
            InetVersion() : version(UNKNOWN) {}
            InetVersion(int version) : version(version) {}
            virtual ~InetVersion() {}
            int getVersion();
            bool operator== (int other) const {return version == other;}
            void operator= (int version) {this->version = version;}
        };
        
    private:
        std::string host;
        int port;
		
        InetVersion inetVersion;
        
    public:
        InetAddress();
        InetAddress(const std::string & host, int port);
		InetAddress(int port);
        InetAddress(struct sockaddr * addr);
        virtual ~InetAddress();
        
		int getFamilyCode() const;
        bool inet4() const;
        bool inet6() const;
        void setInetVersion(int version);
        
        std::string getHost() const;
        void setHost(const std::string & host);
        int getPort() const;
        void setPort(int port);
		void setAddressWithSockAddr(struct sockaddr * addr);
		void setAddress(const InetAddress & addr);
		
		struct addrinfo * resolve(int socktype) const;
		struct addrinfo * resolveNumeric(int socktype) const;
		struct addrinfo * resolvePassive(int family, int socktype) const;

		bool operator==(const InetAddress & other);

		bool valid();

	private:
		static addrinfo * getAddressInfo(const char * node, const char * service, struct addrinfo * hints);
	public:
        
        static std::string getIPAddress(struct sockaddr * addr);
        static int getPort(struct sockaddr * addr);
		static struct addrinfo * getAddrInfo(const char * host, int port, struct addrinfo hints);
    };

	/**
	 * @brief inet 4 address
	 */
	class Inet4Address : public InetAddress {
	private:
	public:
		Inet4Address() { setInetVersion(InetVersion::INET4); }
		Inet4Address(const std::string & host, int port) : InetAddress(host, port) { setInetVersion(InetVersion::INET4); }
		Inet4Address(int port) : InetAddress(port) { setInetVersion(InetVersion::INET4); }
		Inet4Address(struct sockaddr * addr) : InetAddress(addr) { setInetVersion(InetVersion::INET4); }
		virtual ~Inet4Address() {}
	};

	/**
	 * @brief inet 6 address
	 */
	class Inet6Address : public InetAddress {
	private:
	public:
		Inet6Address() { setInetVersion(InetVersion::INET6); }
		Inet6Address(const std::string & host, int port) : InetAddress(host, port) { setInetVersion(InetVersion::INET6); }
		Inet6Address(int port) : InetAddress(port) { setInetVersion(InetVersion::INET6); }
		Inet6Address(struct sockaddr * addr) : InetAddress(addr) { setInetVersion(InetVersion::INET6); }
		virtual ~Inet6Address() {}
	};

	/*
	 * @brief SocketAddress
	 */
	class SocketAddress {
	private:
		struct sockaddr_in in4;
		struct sockaddr_in6 in6;
		struct sockaddr * in;
		socklen_t len;
	public:
		SocketAddress();
		SocketAddress(int spec);
		virtual ~SocketAddress();

		void select(int spec);
		struct sockaddr * getAddr();
		socklen_t * getAddrLen();
	};
    
    /**
     * @brief network interface
     */
    class NetworkInterface {
	private:
		std::string name;
		std::string description;
		std::vector<InetAddress> inetAddresses;
		char mac_address[6];
		bool loopback;
	public:
		NetworkInterface(const std::string & name);
		virtual ~NetworkInterface();
		std::string getName() const;
		void setDescription(const std::string & description);
		std::string getDescription() const;
		void setInetAddress(const InetAddress & address);
		std::vector<InetAddress> getInetAddresses();
		const std::vector<InetAddress> getInetAddresses() const;
		void setMacAddress(const unsigned char * mac_address, size_t size);
		void getMacAddress(unsigned char * out, size_t size) const;
		void setLoopBack(bool loopback);
		bool isLoopBack() const;
    };

	/**
	 * @brief network
	 */
	class Network {
	private:
	public:
        static std::vector<InetAddress> getInetAddressesWithIfaceName(const std::string & ifaceName);
        static std::vector<NetworkInterface> getNetworkInterfaces();
		static std::vector<InetAddress> getAllInetAddress();
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
		virtual bool isSelected(Selector & selector);
		virtual bool isReadableSelected(Selector & selector);
		virtual bool isWritableSelected(Selector & selector);
		virtual bool isExceptSelected(Selector & selector);
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
		virtual bool isSelected(int fd);
		virtual bool isSelected(Selectable & selectable);
		virtual bool isReadableSelected(int fd);
		virtual bool isReadableSelected(Selectable & selectable);
		virtual bool isWritableSelected(int fd);
		virtual bool isWritableSelected(Selectable & selectable);
		virtual bool isExceptSelected(int fd);
		virtual bool isExceptSelected(Selectable & selectable);
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
		virtual bool isSelected(int fd);
		virtual bool isSelected(Selectable & selectable);
		virtual bool isReadableSelected(int fd);
		virtual bool isReadableSelected(Selectable & selectable);
		virtual bool isWritableSelected(int fd);
		virtual bool isWritableSelected(Selectable & selectable);
		virtual bool isExceptSelected(int fd);
		virtual bool isExceptSelected(Selectable & selectable);
	};
	

	/**
	 * @brief socket util
	 */
	class SocketUtil {
	private:
	public:
		SocketUtil();
		virtual ~SocketUtil();
		static void checkValidSocket(SOCK_HANDLE sock);
		static bool isValidSocket(SOCK_HANDLE sock);
		static void throwSocketException(const std::string & message);
		static void closeSocket(SOCK_HANDLE sock);
        static void setSocketOption(SOCK_HANDLE sock, int level, int optname, const char * optval, int optlen);
	};

    /**
     * @brief RandomPortBinder
     */
    class RandomPortBinder {
    public:
        RandomPortBinder() {}
        virtual ~RandomPortBinder() {}
        virtual void start() = 0;
        virtual int getNextPort() = 0;
        virtual bool wantFinish() = 0;
        virtual int getSelectedPort() = 0;
    };

	/**
     * @brief GlobalSocketConfiguration
     */
	class GlobalSocketConfiguration {
	private:
		static int preferredInetVersion;
	private:
		GlobalSocketConfiguration();
		virtual ~GlobalSocketConfiguration();

	public:
		static int getPreferredInetVersion();
		static void setPreferredInetVersion(int preferredInetVersion);
	};

	/**
     * @brief SocketOptions
     */
	class SocketOptions {
	private:
		SocketOptions * delegator;
		bool reuseAddr;
		bool broadcast;
		int ttl;
		std::string multicastIface;
	public:
		SocketOptions();
		virtual ~SocketOptions();
		void setDelegator(SocketOptions * delegator);
		virtual void setReuseAddr(bool reuseAddr);
		virtual bool getReuseAddr();
		virtual void setBroadcast(bool broadcast);
		virtual bool getBroadcast();
		virtual void setTimeToLive(int ttl);
		virtual int getTimeToLive();
		virtual void setMulticastInterface(const std::string & iface);
	};

	/**
	 *
	 */
	class SocketAddressResolver {
	private:
		struct addrinfo * info;
	public:
		SocketAddressResolver();
		virtual ~SocketAddressResolver();
		bool resolved();
		void releaseAddrInfo();
		void setAddrInfo(struct addrinfo * info);
		struct addrinfo * getAddrInfo();
	};
	
	/**
	 * @brief Datagram packet
	 */
	class DatagramPacket {
	private:
		char * data;
		size_t size;
		size_t length;

		InetAddress remoteAddr;
        
	public:
		DatagramPacket(char * data, size_t size);
		DatagramPacket(char * data, size_t size, const InetAddress & remoteAddr);
		DatagramPacket(char * data, size_t size, const std::string & host, int port);
		virtual ~DatagramPacket();
		void clear();
		char * getData();
        const char * getData() const;
        size_t getLength() const;
        size_t getSize() const;
		void write(const char * data, size_t size);
		void write(const std::string & data);
		void setLength(size_t length);
		InetAddress & getRemoteAddr();
		void setRemoteAddr(const InetAddress & addr);
	};
	
}

#endif
