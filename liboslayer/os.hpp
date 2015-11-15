#ifndef __OS_HPP__
#define __OS_HPP__

/*
 * @brief standard header
 */
#include <string>
#include <vector>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <exception>

/**
 * @brief common feature
 * unused : http://stackoverflow.com/questions/3599160/unused-parameter-warnings-in-c-code
 */
#define SUPPRESS_WARNING(x) (void)(x)

/*
* Predefined OS macros
* ====================
* http://stackoverflow.com/questions/142508/how-do-i-check-os-with-a-preprocessor-directive
*/

#if defined(__APPLE__) || defined(__MACH__) /* mac os x */

#	define USE_UNIX_STD
#	define USE_APPLE_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>

#	define TIME long int

#elif defined(unix) || defined(__unix__) || defined(__unix) /* unix or linux */

#	define USE_UNIX_STD
#	define USE_POSIX_SEMAPHORE
#	define USE_PTHREAD
#	define USE_BSD_SOCKET

#	if !defined(__CYGWIN__)
#		define USE_PRCTL
#	endif

#	include <unistd.h>
#	include <sys/time.h>
#	include <sys/stat.h>
#	include <sys/types.h>
#	include <ctime>
#	include <dirent.h>

#	define TIME long int

#elif defined(_WIN32) || defined(_WIN64) /* windows */

// #	define _WIN32_WINNT 0x501 /* force to use windows xp APIs */

#	define USE_MS_WIN
#	define USE_WIN_THREAD
#	define USE_WINSOCK2

#	define snprintf _snprintf_s
#	define strncpy strncpy_s

#	define TIME FILETIME

#	include <direct.h>
#	include <sys/stat.h>
#	include <io.h>

#	define __func__ __FUNCTION__

#endif /* OS DETECTING */



/*
 * Semaphore
 */

#if defined(USE_APPLE_SEMAPHORE)

#	include <dispatch/dispatch.h>
typedef dispatch_semaphore_t SEM_HANDLE;

#elif defined(USE_POSIX_SEMAPHORE)

#	include <semaphore.h>
typedef sem_t SEM_HANDLE;

#else

#	define SEM_HANDLE HANDLE // TODO: windows how to?

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

typedef int SOCK_HANDLE;

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

#define DECL_NAMED_ONLY_EXCEPTION(NAME) class NAME : public Exception { \
public: \
    NAME() { \
    } \
    NAME(const std::string & message, int errorCode, int subErrorCode) \
    : Exception(message, errorCode, subErrorCode) { \
    } \
    NAME(const char * message, int errorCode, int subErrorCode) \
    : Exception(message, errorCode, subErrorCode) { \
    } \
    virtual ~NAME() { \
    } \
}

	/**
	 * @brief Exception
	 */
	class Exception {
	private:
		std::string message;
		int errorCode;
		int subErrorCode;
	public:
		Exception() : errorCode(-1), subErrorCode(-1) {
		}
		Exception(const std::string & message, int errorCode, int subErrorCode) :
			message(message), errorCode(errorCode), subErrorCode(subErrorCode) {
		}
		Exception(const char * message, int errorCode, int subErrorCode) :
			message(message), errorCode(errorCode), subErrorCode(subErrorCode) {
		}
		virtual ~Exception() {
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
	};
    
    DECL_NAMED_ONLY_EXCEPTION(IOException);
    DECL_NAMED_ONLY_EXCEPTION(NotImplementedException);

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
    };

	/**
	 * @brief semaphore
	 */
	class Semaphore {
	private:
		int initial;
		SEM_HANDLE handle;
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
	private: /* private */
		
		THREAD_HANDLE handle;
		bool signal_interrupt;
		bool running;

	private: /* private static */
		
		static unsigned int s_thread_id_seed;

	public: /* public */
		
		unsigned int id;
		
	public: /* methods */
		
		Thread();
		virtual ~Thread();

		unsigned int getId();

		void reset();

		bool start();
		void interrupt();
		bool interrupted();
		bool isRunning();

		void join();

		virtual void run() = 0;
	};
    
    /**
     * @brief InetAddress
     */
    class InetAddress {
    public:
        class InetVersion {
        public:
            static const int UNKNOWN = 0;
            static const int INET4 = 1;
            static const int INET6 = 2;
        private:
            unsigned long version;
        public:
            InetVersion() : version(UNKNOWN) {}
            InetVersion(int version) : version(version) {}
            virtual ~InetVersion() {}
            int getVersion();
            bool operator== (int other) const {return version == other;}
            void operator= (int version) {this->version = version;}
        };
        
    private:
        std::string address;
        int port;
        
        InetVersion inetVersion;
        
    public:
        InetAddress();
        InetAddress(const std::string & address, int port);
        InetAddress(sockaddr_in * addr);
        virtual ~InetAddress();
        
        bool inet4();
        bool inet6();
        void setInetVersion(int version);
        
        std::string getAddress() const;
        void setAddress(const std::string & address);
        int getPort() const;
        void setPort(int port);
        
        static std::string getIPAddress(sockaddr_in * addr);
        static int getPortNumber(sockaddr_in * addr);
        
    };
    
    /**
     * @brief network interface
     */
    class NetworkInterface {
    private:
        std::string name;
		std::string description;
        std::vector<InetAddress> inetAddresses;
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
        
		void setLoopBack(bool loopback);
        bool isLoopBack();
        
    };

	/**
	 * @brief network
	 */
	class Network {
	private:
	public:
        static std::vector<InetAddress> getInetAddressesWithIfaceName(const std::string & ifaceName);
        static std::vector<NetworkInterface> getNetworkInterfaces();

	};
    
    /**
     * @brief selection
     */
    class Selection {
        
    private:
        int fd;
        bool readable;
        bool writeable;
        
    public:
        Selection(int fd, bool readable, bool writeable);
        virtual ~Selection();
        
        int getFd();
        bool isReadable();
        bool isWritable();
    };
    
    class Selector;
    
    /**
     * @brief Selectable interface
     */
    class Selectable {
    public:
        Selectable() {}
        virtual ~Selectable() {}
        
        virtual int getFd() = 0;
        virtual void registerSelector(Selector & selector) = 0;
        virtual void unregisterSelector(Selector & selector) = 0;
        virtual bool isSelected(Selector & selector) = 0;
    };

	/**
	 * @brief selector
	 */
	class Selector {
	private:
		int maxfds;
        fd_set readfds;
        fd_set writefds;
		fd_set curreadfds;
        fd_set curwritefds;
		std::vector<Selection> selected;
        
	public:
		Selector();
		virtual ~Selector();

		virtual void set(int fd);
		virtual void unset(int fd);
		virtual int select(unsigned long timeout_milli);
		virtual std::vector<Selection> & getSelected();
		virtual bool isSelected(int fd);
        virtual bool isSelected(Selectable & selectable);
        virtual bool isReadableSelected(int fd);
        virtual bool isReadableSelected(Selectable & selectable);
        virtual bool isWriteableSelected(int fd);
        virtual bool isWriteableSelected(Selectable & selectable);
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
	};

    /**
     * @brief socket binder
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

	/*
	 * @brief Socket
	 */
	class Socket : public Selectable {
	private:
		SOCK_HANDLE sock;
		Socket * socketImpl;
		char host[2048];
		int port;
		
	protected:
		Socket();

	public:
		Socket(SOCK_HANDLE sock);
		Socket(const char * host, int port);
		virtual ~Socket();

	protected:
		void init();
		void setAddress(const char * host, int port);
		void setHost(const char * host);

	public:
		virtual int connect();

		virtual void registerSelector(Selector & selector);
        virtual void unregisterSelector(Selector & selector);
        virtual bool isSelected(Selector & selector);
        
		virtual bool compareFd(int fd);
		virtual int getFd();

		virtual int recv(char * buffer, size_t max);
		virtual int send(const char * buffer, size_t length);

		virtual void shutdown(/* type */);
		virtual void close();
		virtual bool isClosed();

		char * getHost();
		int getPort();

		SOCK_HANDLE socket();

	protected:
		void socket(SOCK_HANDLE sock);
	
	};

	/*
	 * @brief Server Socket
	 */
	class ServerSocket : public Selectable {
	private:
		SOCK_HANDLE sock;
		ServerSocket * serverSocketImpl;
		int port;
		
	protected:
		ServerSocket();
		
	public:
		ServerSocket(int port);
		virtual ~ServerSocket();

	protected:
		void init();
		void setPort(int port);

	public:
		virtual void setReuseAddr();

		virtual void registerSelector(Selector & selector);
        virtual void unregisterSelector(Selector & selector);
        virtual bool isSelected(Selector & selector);
        
		virtual bool compareFd(int fd);
		virtual int getFd();

		virtual int bind();
        virtual int randomBind(RandomPortBinder & portBinder);
		virtual int listen(int max);
		virtual Socket * accept();
		virtual void close();
		virtual bool isClosed();

		int getPort();

		SOCK_HANDLE socket();

	protected:
		void socket(SOCK_HANDLE sock);

	};

	/**
	 * @brief Datagram packet
	 */
	class DatagramPacket {
	private:
		char * data;
		size_t length;
		size_t maxSize;
		std::string remoteAddr;
		int remotePort;
        
	public:
		DatagramPacket(char * data, size_t maxSize);
		virtual ~DatagramPacket();
		char * getData();
        const char * getData() const;
		size_t getLength();
        size_t getLength() const;
		size_t getMaxSize();
        size_t getMaxSize() const;
		void setLength(size_t length);
		std::string getRemoteAddr();
        std::string getRemoteAddr() const;
		int getRemotePort();
        int getRemotePort() const;
		void setRemoteAddr(std::string remoteAddr);
		void setRemotePort(int remotePort);
	};

	/*
	 * @brief Datagram Socket
	 */
	class DatagramSocket : public Selectable {
	private:
		SOCK_HANDLE sock;
		DatagramSocket * socketImpl;
		char host[2048];
		int port;
		
	protected:
		DatagramSocket();

	public:
		DatagramSocket(int port);
		DatagramSocket(const char * host, int port);
		virtual ~DatagramSocket();

	protected:
		void init();
		void setPort(int port);
		void setAddress(const char * host, int port);
		void setHost(const char * host);

	public:
		virtual void setReuseAddr();
		virtual void setBroadcast();
		virtual int joinGroup(const std::string & group);
		virtual int joinGroup(const char * group);
		virtual int bind();
        virtual int randomBind(RandomPortBinder & portBinder);
		virtual int connect();

		virtual void registerSelector(Selector & selector);
        virtual void unregisterSelector(Selector & selector);
        virtual bool isSelected(Selector & selector);
        
		virtual bool compareFd(int fd);
		virtual int getFd();

		virtual int recv(DatagramPacket & packet);
		virtual int recv(char * buffer, size_t max);
		virtual int send(const char * host, int port, const char * buffer, size_t length);

		virtual void shutdown(/* type */);
		virtual void close();
		virtual bool isClosed();

		char * getHost();
		int getPort();

		SOCK_HANDLE socket();

	protected:
		void socket(SOCK_HANDLE sock);
	};
	


	/**
	 * @brief date
	 */
	class Date {
	private:
	public:
		static std::string DEFAULT_FORMAT;
	public:
		static std::string format(const std::string & fmt, TIME seconds);
	};

	/**
	 * @brief file
	 */
	class File {
	private:
		std::string path;

	public:
		File();
		File(const std::string & path);
		virtual ~File();

		static std::string mergePaths(const std::string & dir, const std::string & filename);
		static std::string mergePaths(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string fullpath(const std::string & dir, const std::string & filename);
		static std::string fullpath(const std::string & dir, const std::string & filename, const std::string & separators);
		static std::string getCwd();

		/* */
		static bool isRootPath(const std::string & path);
		static bool isFullpath(const std::string & path);
		static bool exists(const std::string & path);
		static bool isFile(const std::string & path);
		static bool isDirectory(const std::string & path);
		static bool isWritable(const std::string & path);
		static std::string getParentPath(const std::string & path);
		static std::string getPathPart(const std::string & path);
		static std::string getFileNamePart(const std::string & path);
		static std::string getExtension(const std::string & path);
		static std::string getEntityNamePart(const std::string & path);
		static bool compareExtension(const std::string & path, std::string extension);
		static int mkdir(const std::string & path);
		static std::string getCreationDate(const std::string & path, std::string fmt = Date::DEFAULT_FORMAT);
		static std::string getModifiedDate(const std::string & path, std::string fmt = Date::DEFAULT_FORMAT);
		static std::vector<File> list(const std::string & path);

		std::string getName();
		virtual std::string toString();
		std::string getPath();

		bool isRootPath();
		bool isFullpath();
		bool exists();
		bool isFile();
		bool isDirectory();
		bool isWritable();
		std::string getParentPath();
		std::string getPathPart();
		std::string getFileNamePart();
		std::string getExtension();
		std::string getEntityNamePart();
		bool compareExtension(std::string extension);
		int mkdir();
		std::string getCreationDate(const std::string & fmt = Date::DEFAULT_FORMAT);
		std::string getModifiedDate(const std::string & fmt = Date::DEFAULT_FORMAT);
		std::vector<File> list();
	};
	
}

#endif
