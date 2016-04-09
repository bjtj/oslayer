#include "os.hpp"

#define CHECK_NOT_IMPL_THROW(x) if(!x){throw NotImplementedException();}

/**
 * @namespace OS
 */
namespace OS {

	using namespace std;

	string nomeaningfulVesion() {
		return "0.1";
	}

	/**
	 * @brief milliseconds sleep
	 */
	void idle(unsigned long timeout) {
        
#if defined(USE_UNIX_STD)
				
		struct timespec ts;
		ts.tv_sec = timeout / 1000;
		ts.tv_nsec = (timeout % 1000) * (1000 * 1000);
		nanosleep(&ts, NULL);
        
#elif defined(USE_MS_WIN)
        
		Sleep(timeout);
        
#else
        
		// sleep
        
#endif
	}

	/**
	 * @brief get tick count
	 */
	unsigned long tick_milli() {
        
#if defined(USE_APPLE_STD)
        
        // @ref http://stackoverflow.com/a/11681069
        
        clock_serv_t cclock;
        mach_timespec_t mts;
        host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
        clock_get_time(cclock, &mts);
        mach_port_deallocate(mach_task_self(), cclock);
        return (mts.tv_sec * 1000) + (mts.tv_nsec / 1000000);
        
#elif defined(USE_POSIX_STD)

		struct timespec spec;
		clock_gettime(CLOCK_MONOTONIC, &spec);
		return (spec.tv_sec * 1000) + (spec.tv_nsec / 1000000);
		
#elif defined(USE_MS_WIN)
        
		return GetTickCount();
        
#else
        
		throw NotImplementedException("not implemented");
        
#endif	
	}

	/**
	 * @brief Library
	 */

	static string s_to_lib_name(const string & name) {
#if defined(USE_APPLE_STD)
		return "lib" + name + ".dylib";
#elif defined(USE_UNIX_STD)
		return "lib" + name + ".so";
#elif defined(USE_MS_WIN)
		return name + ".dll";
#endif
	}

	Library::Library(const string & path, const string name) : path(path), name(name) {
		string fullpath = File::mergePaths(path, s_to_lib_name(name));
#if defined(USE_UNIX_STD)
		handle = dlopen(fullpath.c_str(), RTLD_LAZY);
#elif defined(USE_MS_WIN)
		handle = LoadLibrary(fullpath.c_str());
#endif
	}
	Library::~Library() {
#if defined(USE_UNIX_STD)
		dlclose(handle);
#elif defined(USE_MS_WIN)
		FreeLibrary(handle);
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
	SYM_HANDLE Library::getSymbol(const std::string & sym) {
#if defined(USE_UNIX_STD)
		return dlsym(handle, sym.c_str());
#elif defined(USE_MS_WIN)
		return GetProcAddress(handle, sym.c_str());
#endif
	}


    /**
     * @brief System
     */
    
    class SystemImpl : public System {
    public:
        SystemImpl () {
        }
        virtual ~SystemImpl() {
        }
		virtual void ignoreSigpipe() {
		}
    };
    
#if defined(USE_UNIX_STD)
	
    class NixSystemImpl : public SystemImpl {
    public:
        NixSystemImpl() {
        }
        virtual ~NixSystemImpl() {
        }
		virtual void ignoreSigpipe() {
#if defined(USE_SIGNAL)
			signal(SIGPIPE, SIG_IGN);
#endif
		}
    };
    static NixSystemImpl s_systemImpl;
	
#elif defined(USE_MS_WIN)
	
    class MSSystemImpl : public SystemImpl {
    public:
        MSSystemImpl() {
            WSADATA wsaData;
            if (WSAStartup(MAKEWORD(2,2), &wsaData) != 0) {
				throw Exception("WSAStartup() failed", -1, 0);
			}
        }
        virtual ~MSSystemImpl() {
            WSACleanup();
        }
    };
    static MSSystemImpl s_systemImpl;
	
#else
	
    static SystemImpl s_systemImpl;
	
#endif
    
    System * System::systemImpl = &s_systemImpl;
    
    System::System() {
    }
    System::~System() {
    }
    System * System::getInstance() {
        return systemImpl;
    }
    
    
	/* SEMAPHORE */

#if defined(USE_APPLE_SEMAPHORE)

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		*handle = dispatch_semaphore_create(initial);
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		dispatch_semaphore_wait(*handle, DISPATCH_TIME_FOREVER);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		dispatch_semaphore_signal(*handle);
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		SUPPRESS_WARNING(handle);
		// ???
	}

#elif defined(USE_POSIX_SEMAPHORE)

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		sem_init(handle, 0, initial);
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		sem_wait(handle);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		sem_post(handle);
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		sem_destroy(handle);
	}

#else

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		*handle = CreateSemaphore(
			NULL,		// default security attributes
			initial,	// initial count
			initial,	// maximum count
			NULL);		// unnamed semaphore
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		WaitForSingleObject(*handle, INFINITE);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		ReleaseSemaphore( 
                        *handle,	// handle to semaphore
                        1,			// increase count by one
                        NULL);		// not interested in previous count
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		CloseHandle(*handle);
	}

#endif /* SEMAPHORE */
	

	Semaphore::Semaphore(int initial) : initial(initial) {
		s_sem_init(&handle, initial);
	}

	Semaphore::Semaphore(const Semaphore & other) {
		this->initial = other.initial;
		s_sem_init(&(this->handle), this->initial);
	}
	
	Semaphore::~Semaphore() {
		s_sem_destroy(&handle);
	}
	
	void Semaphore::wait() const {
		s_sem_wait(&handle);
	}
	
	void Semaphore::post() const {
        s_sem_post(&handle);
	}

    
    /**
     * @breif auto lock
     */
    
    AutoLock::AutoLock(Semaphore & sem) : sem(sem) {
        sem.wait();
    }
    AutoLock::~AutoLock() {
        sem.post();
    }
    

	/* THREAD */
#if defined(USE_PTHREAD)

	// unix or linux

	typedef void * (*thread_func)(void *);
	
	static void * s_thread_wrapper(void * arg) {

		
		Thread * thread = (Thread*)arg;

#if defined(USE_PRCTL)
		{
			char name[16] = {0,};
			snprintf(name, sizeof(name), "tid:0x%x", (unsigned int)thread->getId());
			prctl(PR_SET_NAME, name, 0, 0, 0);
		}
#endif

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief pthread_create
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread) {
	
		pthread_attr_t attr;
		bool started = false;

		if (!handle || !thread) {
			return false;
		}

		if (pthread_attr_init(&attr) != 0) {
			return false;
		}

		if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0) {
			return false;
		}
			
		if (pthread_create(handle, &attr, func, (void*)thread) == 0) {
			started = true;
		}

		pthread_attr_destroy(&attr);
		return started;
	}

#elif defined(USE_WIN_THREAD)

	// windows

	typedef UINT (WINAPI thread_func)(void *);

	static UINT WINAPI s_thread_wrapper(void * arg) {

		Thread * thread = (Thread*)arg;

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief win32 thread start
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread) {
		UINT dwThreadID;
		HANDLE h;

		if (!handle || !thread) {
			return false;
		}

		h = (HANDLE)_beginthreadex(NULL, 0, func, (void*)thread, 0, &dwThreadID);
		*handle = h;
		
		return false;
	}

#endif

	/**
	 * @brief thread id seed - 0 means error
	 */
	unsigned int Thread::s_thread_id_seed = 0;

	/**
	 * @brief thread constructor
	 */
	Thread::Thread() : handle(0), signal_interrupt(false) {
		id = (++s_thread_id_seed == 0) ? ++s_thread_id_seed : s_thread_id_seed;

		reset();
	}
	
	Thread::~Thread() {
	}

	unsigned int Thread::getId() {
		return id;
	}

	void Thread::reset() {
		signal_interrupt = false;
		running = false;
	}

	bool Thread::start() {
		
		if (!isRunning()) {
			bool ret = s_startThread(&handle, s_thread_wrapper, this);
			running = true;
			return ret;
		}
		return false;
	}
	
	void Thread::interrupt() {
		signal_interrupt = true;
	}

	bool Thread::interrupted() {
		bool ret = signal_interrupt;
		signal_interrupt = false;
		return ret;
	}

	bool Thread::isRunning() {
		return running;
	}

	void Thread::join() {
		while (running) { /* take time : 10ms */ idle(10); }
	}
    
    
    /* InetAddress */
    
    InetAddress::InetAddress() : port(-1) {
    }
    InetAddress::InetAddress(const string & host, int port) : host(host), port(port) {
    }
	InetAddress::InetAddress(int port) : port(port) {
	}
    InetAddress::InetAddress(struct sockaddr * addr) : port(-1) {
        setAddressWithSockAddr(addr);
    }
    InetAddress::~InetAddress() {
    }
	int InetAddress::getFamilyCode() const {
		if (inet4()) {
			return AF_INET;
		}
		if (inet6()) {
			return AF_INET6;
		}
		return AF_UNSPEC;
	}
    bool InetAddress::inet4() const {
        return inetVersion == InetVersion::INET4;
    }
    bool InetAddress::inet6() const {
        return inetVersion == InetVersion::INET6;
    }
    void InetAddress::setInetVersion(int version) {
        inetVersion = version;
    }
    string InetAddress::getHost() const {
        return host;
    }
    void InetAddress::setHost(const string & host) {
        this->host = host;
    }
    int InetAddress::getPort() const {
        return port;
    }
    void InetAddress::setPort(int port) {
        this->port = port;
    }
	void InetAddress::setAddressWithSockAddr(struct sockaddr * addr) {
		if (addr->sa_family == AF_INET) {
            setInetVersion(InetAddress::InetVersion::INET4);
		} else if (addr->sa_family == AF_INET6) {
            setInetVersion(InetAddress::InetVersion::INET6);
        } else {
			throw IOException("Unknown spec", -1, 0);
		}
        host = InetAddress::getIPAddress(addr);
		port = InetAddress::getPort(addr);
	}
	void InetAddress::setAddress(const InetAddress & addr) {
		this->host = addr.host;
		this->port = addr.port;
	}
	
	struct addrinfo * InetAddress::resolve(int socktype) const {

		char portStr[10] = {0,};
		snprintf(portStr, sizeof(portStr), "%d", port);

		struct addrinfo hints;
		memset(&hints, 0, sizeof(hints));
		hints.ai_family = AF_UNSPEC;
		hints.ai_socktype = socktype;
		return getAddressInfo((host.empty() ? NULL : host.c_str()), (port == -1 ? NULL : portStr), &hints);
	}
	struct addrinfo * InetAddress::resolveNumeric(int socktype) const {

		char portStr[10] = {0,};
		snprintf(portStr, sizeof(portStr), "%d", port);

		struct addrinfo hints;
		memset(&hints, 0, sizeof(hints));
		hints.ai_family = AF_UNSPEC;
		hints.ai_socktype = socktype;
		hints.ai_flags = AI_NUMERICHOST;
		return getAddressInfo((host.empty() ? NULL : host.c_str()), (port == -1 ? NULL : portStr), &hints);
	}
	struct addrinfo * InetAddress::resolvePassive(int family, int socktype) const {

		char portStr[10] = {0,};
		snprintf(portStr, sizeof(portStr), "%d", port);

		struct addrinfo hints;
		memset(&hints, 0, sizeof(hints));
		hints.ai_family = family;
		hints.ai_socktype = socktype;
		hints.ai_flags = AI_PASSIVE;
		
		return getAddressInfo((host.empty() ? NULL : host.c_str()), (port == -1 ? NULL : portStr), &hints);
	}

	addrinfo * InetAddress::getAddressInfo(const char * node, const char * service, struct addrinfo * hints) {
		struct addrinfo * res;
		if (getaddrinfo(node, service, hints, &res) != 0) {
			SocketUtil::throwSocketException("getaddrinfo() error");
		}
		return res;
	}

    string InetAddress::getIPAddress(struct sockaddr * addr) {
        char ipstr[INET6_ADDRSTRLEN] = {0,};
        
#if (!defined(USE_WINSOCK2) || _WIN32_WINNT >= 0x0600)

		inet_ntop(addr->sa_family, (addr->sa_family == AF_INET ?
			(void*)&((struct sockaddr_in*)addr)->sin_addr : (void*)&((struct sockaddr_in6 *)addr)->sin6_addr),
			ipstr, sizeof(ipstr));
        
#else
        char * ptr = NULL;
        if ((ptr = inet_ntoa(addr->sin_addr)) != NULL) {
            strncpy(ipstr, ptr, INET6_ADDRSTRLEN - 1);
        }
#endif
        
        return string(ipstr);
    }
    int InetAddress::getPort(struct sockaddr * addr) {
        return ntohs((addr->sa_family == AF_INET ?
                      ((struct sockaddr_in*)addr)->sin_port: ((struct sockaddr_in6 *)addr)->sin6_port));
    }
	struct addrinfo * InetAddress::getAddrInfo(const char * host, int port, struct addrinfo hints) {
		struct addrinfo * res;
		char portStr[10] = {0,};
		snprintf(portStr, sizeof(portStr), "%d", port);
		memset(&hints, 0, sizeof(hints));
		if (getaddrinfo(host, (port == 0 ? NULL : portStr), &hints, &res) != 0) {
			throw OS::IOException("getaddrinfo() error", -1, 0);
		}
		return res;
	}
	
	bool InetAddress::operator==(const InetAddress & other) {
		return (!host.compare(other.host) && port == other.port);
	}

	bool InetAddress::valid() {
		// TODO:
		if (port < 0) {
			return false;
		}
		return true;
	}

	/* Socket Address */

	SocketAddress::SocketAddress() : in(NULL), len(0) {
	}
	SocketAddress::SocketAddress(int spec) : in(NULL), len(0) {
		select(spec);
	}
	SocketAddress::~SocketAddress() {
	}

	void SocketAddress::select(int spec) {
		if (spec == AF_INET) {
			in = (struct sockaddr *)&in4;
			len = sizeof(in4);
		} else if (spec == AF_INET6) {
			in = (struct sockaddr *)&in6;
			len = sizeof(in6);
		} else {
			throw OS::IOException("Unknown family", -1, 0);
		}
	}
	struct sockaddr * SocketAddress::getAddr() {
		return in;
	}
	socklen_t * SocketAddress::getAddrLen() {
		return &len;
	}
    
    /* Network Interface */
    
    NetworkInterface::NetworkInterface(const string & name) : name(name), loopback(false) {
    }
    NetworkInterface::~NetworkInterface() {
    }
    string NetworkInterface::getName() const {
        return name;
    }
	void NetworkInterface::setDescription(const string & description) {
		this->description = description;
	}
	string NetworkInterface::getDescription() const {
		return description;
	}
    void NetworkInterface::setInetAddress(const InetAddress & address) {
        inetAddresses.push_back(address);
    }
    vector<InetAddress> NetworkInterface::getInetAddresses() {
        return inetAddresses;
    }

	const vector<InetAddress> NetworkInterface::getInetAddresses() const {
        return inetAddresses;
    }

	void NetworkInterface::setLoopBack(bool loopback) {
		this->loopback = loopback;
	}

    bool NetworkInterface::isLoopBack() {
		if (loopback) {
			return true;
		}

        for (size_t i = 0; i < inetAddresses.size(); i++) {
            InetAddress & addr = inetAddresses[i];
            if (!addr.getHost().compare("127.0.0.1") ||
                !addr.getHost().compare("::1")) {
                return true;
            }
        }

        return false;
    }
    

	/* Network */
	
#if defined(USE_BSD_SOCKET)
    
    static NetworkInterface & s_obtain_network_interface(vector<NetworkInterface> & ifaces, const string & name) {
        for (size_t i = 0; i < ifaces.size(); i++) {
            if (!ifaces[i].getName().compare(name)) {
                return ifaces[i];
            }
        }
        ifaces.push_back(NetworkInterface(name));
        return s_obtain_network_interface(ifaces, name);
    }
    
    static vector<NetworkInterface> s_get_all_network_interfaces() {
        
        vector<NetworkInterface> ifaces;
        
        struct ifaddrs * addrs, * tmp;
        getifaddrs(&addrs);
        tmp = addrs;
        
        while (tmp) {
            
            if (tmp->ifa_addr && (tmp->ifa_addr->sa_family == AF_INET || tmp->ifa_addr->sa_family == AF_INET6)) {
                NetworkInterface & iface = s_obtain_network_interface(ifaces, tmp->ifa_name);
                iface.setInetAddress(InetAddress((struct sockaddr*)tmp->ifa_addr));
            }
            
            tmp = tmp->ifa_next;
        }
        freeifaddrs(addrs);
        
        return ifaces;
    }

#elif defined(USE_WINSOCK2)

	static vector<NetworkInterface> s_get_all_network_interfaces() {

	vector<NetworkInterface> ret;

	ULONG outBufLen = 0;
	DWORD dwRetVal = 0;
	IP_ADAPTER_INFO * pAdapterInfos = (IP_ADAPTER_INFO*) malloc(sizeof(IP_ADAPTER_INFO));

	// retry up to 5 times, to get the adapter infos needed
	const int retry = 5;
	for (int i = 0; i < retry && (dwRetVal == ERROR_BUFFER_OVERFLOW || dwRetVal == NO_ERROR); ++i) {

		// GetAdaptersInfo: https://msdn.microsoft.com/ko-kr/library/windows/desktop/aa365917%28v=vs.85%29.aspx
		dwRetVal = GetAdaptersInfo(pAdapterInfos, &outBufLen);

		if (dwRetVal == NO_ERROR) {
			break;
		} else if (dwRetVal == ERROR_BUFFER_OVERFLOW) {
			free(pAdapterInfos);
			pAdapterInfos = (IP_ADAPTER_INFO *)malloc(outBufLen);
		} else {
			pAdapterInfos = NULL;
			break;
		}
	}

	if (dwRetVal == NO_ERROR) {

		IP_ADAPTER_INFO* pAdapterInfo = pAdapterInfos;

		while( pAdapterInfo ) {

			NetworkInterface iface(pAdapterInfo->AdapterName);
			iface.setDescription(pAdapterInfo->Description);

			IP_ADDR_STRING * pIpAddress = &(pAdapterInfo->IpAddressList);

			while( pIpAddress != 0 ) {

				InetAddress address;
				address.setHost(pAdapterInfo->IpAddressList.IpAddress.String);
				address.setInetVersion(InetAddress::InetVersion::INET4);
				iface.setInetAddress(address);

				pIpAddress = pIpAddress->Next;
			}
			pAdapterInfo = pAdapterInfo->Next;

			ret.push_back(iface);
		}
	}
	free(pAdapterInfos);

	return ret;
}

#endif
    
    vector<InetAddress> Network::getInetAddressesWithIfaceName(const string & ifaceName) {
        vector<NetworkInterface> ifaces = getNetworkInterfaces();
        for (size_t i = 0; i < ifaces.size(); i++) {
            NetworkInterface & iface = ifaces[i];
            if (!iface.getName().compare(ifaceName)) {
                return iface.getInetAddresses();
            }
        }
        return vector<InetAddress>();
    }
    
    vector<NetworkInterface> Network::getNetworkInterfaces() {
        return s_get_all_network_interfaces();
    }

	vector<InetAddress> Network::getAllInetAddress() {
		vector<InetAddress> ret;
		vector<NetworkInterface> ifaces = getNetworkInterfaces();
		for (size_t i = 0; i < ifaces.size(); i++) {
			vector<InetAddress> addrs = ifaces[i].getInetAddresses();
			for (vector<InetAddress>::iterator iter = addrs.begin(); iter != addrs.end(); iter++) {
				ret.push_back(*iter);
			}
		}
		return ret;
	}
    
    /* SELECTION */
    
    Selection::Selection(int fd, bool readable, bool writeable, bool except)
		: fd(fd), readable(readable), writeable(writeable), except(except) {
        
    }
    Selection::~Selection() {
        
    }
    
    int Selection::getFd() {
        return fd;
    }
    
    bool Selection::isReadable() {
        return readable;
    }
    
    bool Selection::isWritable() {
        return writeable;
    }

	bool Selection::isExcept() {
		return except;
	}

	/* Selectable */
    
    bool Selectable::isSelectable() {
        return selectable;
    }
    void Selectable::setSelectable(bool selectable) {
        this->selectable = selectable;
    }
	void Selectable::registerSelector(Selector & selector, unsigned long flags) {
		selector.set(getFd(), flags);
	}
	void Selectable::unregisterSelector(Selector & selector, unsigned long flags) {
		selector.unset(getFd(), flags);
	}
	bool Selectable::isSelected(Selector & selector) {
		return selector.isSelected(getFd());
	}
	bool Selectable::isReadableSelected(Selector & selector) {
		return selector.isReadableSelected(getFd());
	}
	bool Selectable::isWritableSelected(Selector & selector) {
		return selector.isWritableSelected(getFd());
	}
	bool Selectable::isExceptSelected(Selector & selector) {
		return selector.isExceptSelected(getFd());
	}

	/* Selector */

	Selector::Selector() : maxfds(0) {
		FD_ZERO(&readfds);
        FD_ZERO(&writefds);
		FD_ZERO(&exceptfds);
		FD_ZERO(&curreadfds);
        FD_ZERO(&curwritefds);
		FD_ZERO(&curexceptfds);
	}
	
	Selector::~Selector() {
	}
	void Selector::set(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("invalid fd", fd, 0);
		}
		
		if (fd > maxfds) {
			maxfds = fd;
		}

		if (flags & Selector::READ) {
			FD_SET(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_SET(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_SET(fd, &exceptfds);
		}
	}
	void Selector::unset(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("Invalid fd", fd, 0);
		}

		if (flags & Selector::READ) {
			FD_CLR(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_CLR(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_CLR(fd, &exceptfds);
		}
	}
	int Selector::select(unsigned long timeout_milli) {

		struct timeval timeout;
		timeout.tv_sec = timeout_milli / 1000;
		timeout.tv_usec = (timeout_milli % 1000) * 1000;
		
		curreadfds = readfds;
        curwritefds = writefds;
		curexceptfds = exceptfds;
		
		return ::select(maxfds + 1, &curreadfds, &curwritefds, &curexceptfds, &timeout);
	}
	vector<Selection> & Selector::getSelections() {
		selections.clear();
		for (int i = 0; i < maxfds + 1; i++) {
            
            bool readable = FD_ISSET(i, &curreadfds) ? true : false;
            bool writeable = FD_ISSET(i, &curwritefds) ? true : false;
			bool error = FD_ISSET(i, &curexceptfds) ? true : false;
            
            if (readable || writeable || error) {
                selections.push_back(Selection(i, readable, writeable, error));
            }
		}
		return selections;
	}

	bool Selector::isSelected(int fd) {
        bool readable = FD_ISSET(fd, &curreadfds) ? true : false;
        bool writeable = FD_ISSET(fd, &curwritefds) ? true : false;
		bool error = FD_ISSET(fd, &curexceptfds) ? true : false;
        
        return readable || writeable || error;
	}
    
    bool Selector::isSelected(Selectable & selectable) {
        return isSelected(selectable.getFd());
    }
    
    bool Selector::isReadableSelected(int fd) {
        return FD_ISSET(fd, &curreadfds) ? true : false;
    }
    
    bool Selector::isReadableSelected(Selectable & selectable) {
        return isReadableSelected(selectable.getFd());
    }
    
    bool Selector::isWritableSelected(int fd) {
        return FD_ISSET(fd, &curwritefds) ? true : false;
    }
    
    bool Selector::isWritableSelected(Selectable & selectable) {
        return isWritableSelected(selectable.getFd());
    }

	bool Selector::isExceptSelected(int fd) {
        return FD_ISSET(fd, &curexceptfds) ? true : false;
    }
    
    bool Selector::isExceptSelected(Selectable & selectable) {
        return isExceptSelected(selectable.getFd());
    }

	/**
	 *
	 */
	void SocketUtil::checkValidSocket(SOCK_HANDLE sock) {
		if (!isValidSocket(sock)) {
			throw IOException("invalid socket", -1, 0);
		}
	}

	bool SocketUtil::isValidSocket(SOCK_HANDLE sock) {
#if defined(USE_BSD_SOCKET)
		if (sock < 0) {
            return false;
        }

#elif defined(USE_WINSOCK2)
		if (sock == INVALID_SOCKET) {
			return false;
		}
#endif
		return true;
	}

	void SocketUtil::throwSocketException(const std::string & message) {

#if defined(USE_BSD_SOCKET)
        
        int err = errno;
        char text[1024] = {0,};
        if (strerror_r(err, text, sizeof(text))) {}
        throw IOException(message + " / " + string(text), err, 0);
        
#elif defined(USE_WINSOCK2)
		int err = WSAGetLastError();
		char text[1024];
		FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
						FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, err,
						MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)text, 1024, NULL);
		throw IOException(message + " / " + string(text), err, 0);
#else
		throw IOException("unknown socket exception");
#endif
	}

	void SocketUtil::closeSocket(SOCK_HANDLE sock) {
#if defined(USE_BSD_SOCKET)
		close(sock);
#elif defined(USE_WINSOCK2)
		closesocket(sock);
#endif
	}

    void SocketUtil::setSocketOption(SOCK_HANDLE sock, int level, int optname, const char * optval, int optlen) {
        if (setsockopt(sock, level, optname, optval, optlen) != 0) {
            throw IOException("setsockopt() error", -1, 0);
        }
    }

	/**
	 * @brief scoped connection (auto free)
	 */
    class ScopedConnection {
    private:
        struct addrinfo ** res;
    public:
        ScopedConnection(struct addrinfo ** res) : res(res) {
        }
        virtual ~ScopedConnection() {
            if (*res) {
                freeaddrinfo(*res);
                *res = NULL;
            }
        }
    };

	int GlobalSocketConfiguration::preferredInetVersion = InetAddress::InetVersion::INET4;

	int GlobalSocketConfiguration::getPreferredInetVersion() {
		return preferredInetVersion;
	}

	void GlobalSocketConfiguration::setPreferredInetVersion(int preferredInetVersion) {
		GlobalSocketConfiguration::preferredInetVersion = preferredInetVersion;
	}

	/**
	 * @brief SocketOptions
	 */
	SocketOptions::SocketOptions() : delegator(NULL), reuseAddr(false), broadcast(false), ttl(0) {
	}
	SocketOptions::~SocketOptions() {
	}
	void SocketOptions::setDelegator(SocketOptions * delegator) {
		this->delegator = delegator;
	}
	void SocketOptions::setReuseAddr(bool reuseAddr) {
		if (delegator) {
			delegator->setReuseAddr(reuseAddr);
		}
		this->reuseAddr = reuseAddr;
	}
	bool SocketOptions::getReuseAddr() {
		if (delegator) {
			return delegator->getReuseAddr();
		}
		return reuseAddr;
	}
	void SocketOptions::setBroadcast(bool broadcast) {
		if (delegator) {
			delegator->setBroadcast(broadcast);
		}
		this->broadcast = broadcast;
	}
	bool SocketOptions::getBroadcast() {
		if (delegator) {
			return delegator->getBroadcast();
		}
		return broadcast;
	}
	void SocketOptions::setTimeToLive(int ttl) {
		if (delegator) {
			delegator->setTimeToLive(ttl);
		}
		this->ttl = ttl;
	}
	int SocketOptions::getTimeToLive() {
		if (delegator) {
			return delegator->getTimeToLive();
		}
		return ttl;
	}
	void SocketOptions::setMulticastInterface(const string & iface) {
		if (delegator) {
			delegator->setMulticastInterface(iface);
		}
		this->multicastIface = iface;
	}

	/* */

	SocketAddressResolver::SocketAddressResolver() : info(NULL) {
	}
	SocketAddressResolver::~SocketAddressResolver() {
		releaseAddrInfo();
	}
	bool SocketAddressResolver::resolved() {
		return info != NULL;
	}
	void SocketAddressResolver::releaseAddrInfo() {
		if (this->info) {
			freeaddrinfo(this->info);
		}
	}
	void SocketAddressResolver::setAddrInfo(struct addrinfo * info) {
		releaseAddrInfo();
		this->info = info;
	}
	struct addrinfo * SocketAddressResolver::getAddrInfo() {
		return info;
	}


	/*
	 * Datagram Packet
	 */

	DatagramPacket::DatagramPacket(char * data, size_t size) : data(data), size(size), length(0) {
        System::getInstance();
	}

	DatagramPacket::DatagramPacket(char * data, size_t size, const InetAddress & remoteAddr)
		: data(data), size(size), length(0), remoteAddr(remoteAddr) {
		System::getInstance();
	}
	DatagramPacket::DatagramPacket(char * data, size_t size, const std::string & host, int port)
		: data(data), size(size), length(0), remoteAddr(host, port) {
		System::getInstance();
	}

	DatagramPacket::~DatagramPacket() {
	}

	void DatagramPacket::clear() {
		memset(data, 0, size);
	}

	char * DatagramPacket::getData() {
		return data;
	}
    
    const char * DatagramPacket::getData() const {
        return data;
    }

    size_t DatagramPacket::getLength() const {
        return length;
    }

    size_t DatagramPacket::getSize() const {
        return size;
    }
	void DatagramPacket::write(const char * data, size_t size) {
		if (this->size < size) {
			throw BufferOverflowException("buffer overflowed", -1, 0);
		}
		memcpy(this->data, data, size);
		setLength(size);
	}
	void DatagramPacket::write(const std::string & data) {
		write(data.c_str(), data.size());
	}
	void DatagramPacket::setLength(size_t length) {
		this->length = length;
	}
	
	InetAddress & DatagramPacket::getRemoteAddr() {
		return remoteAddr;
	}
	void DatagramPacket::setRemoteAddr(const InetAddress & addr) {
		this->remoteAddr.setAddress(addr);
	}
	
	/* Date */
	
#if defined(USE_UNIX_STD)
	static string s_date_format(const string & fmt, TIME time) {
		char buffer[1024] = {0,};
		strftime(buffer, sizeof(buffer), fmt.c_str(), gmtime((time_t *)&time));

		return string(buffer);
	}
#elif defined(USE_MS_WIN)
	static string s_date_format(const string & fmt, TIME time) {
		char buffer[1024] = {0,};
		// SYSTEMTIME stUTC;
		// FileTimeToSystemTime(&time, &stUTC);
		// snprintf(buffer, sizeof(buffer), "%d-%02d-%02d %02d:%02d:%02d", stUTC.wYear, stUTC.wMonth, stUTC.wDay, stUTC.wHour, stUTC.wMinute, stUTC.wSecond);
		snprintf(buffer, sizeof(buffer), "%d-%02d-%02d %02d:%02d:%02d", time.wYear, time.wMonth, time.wDay, time.wHour, time.wMinute, time.wSecond);
		return string(buffer);
	}
#else
	// sleep
#endif

	string Date::DEFAULT_FORMAT = "%Y-%m-%d %H:%M:%S";

	Date::Date() : year(0), month(0), day(0), hour(0), minute(0), second(0), millisecond(0) {
	}
	Date::~Date() {
	}
	Date Date::now() {
		Date date;

#if defined(USE_APPLE_STD)
        
        // @ref http://stackoverflow.com/a/11681069
        
        clock_serv_t cclock;
        mach_timespec_t mts;
        host_get_clock_service(mach_host_self(), CALENDAR_CLOCK, &cclock);
        clock_get_time(cclock, &mts);
        mach_port_deallocate(mach_task_self(), cclock);
        time_t t = (time_t)mts.tv_sec;
        struct tm info;
        localtime_r(&t, &info);
        date.setYear(1900 + info.tm_year);
        date.setMonth(1 + info.tm_mon);
        date.setDay(info.tm_mday);
        date.setHour(info.tm_hour);
        date.setMinute(info.tm_min);
        date.setSecond(info.tm_sec);
        date.setMillisecond(mts.tv_nsec / 1000000);
        
#elif defined(USE_POSIX_STD)
        
        struct timespec spec;
        clock_gettime(CLOCK_REALTIME, &spec);
        time_t t = (time_t)spec.tv_sec;
        struct tm info;
        localtime_r(&t, &info);
        date.setYear(1900 + info.tm_year);
        date.setMonth(1 + info.tm_mon);
        date.setDay(info.tm_mday);
        date.setHour(info.tm_hour);
        date.setMinute(info.tm_min);
        date.setSecond(info.tm_sec);
        date.setMillisecond(spec.tv_nsec / 1000000);
        
#elif defined(USE_MS_WIN)
        
        SYSTEMTIME now;
        GetLocalTime(&now);
		date.setYear(now.wYear);
		date.setMonth(now.wMonth);
		date.setDay(now.wDay);
		date.setHour(now.wHour);
		date.setMinute(now.wMinute);
		date.setSecond(now.wSecond);
		date.setMillisecond(now.wMilliseconds);
        
#endif
		return date;
	}

	/**
	 * @brief seconds to string
	 * @ref http://stackoverflow.com/questions/10446526/get-last-modified-time-of-file-in-linux
	 */
	string Date::format(const string & fmt, TIME seconds) {
		return s_date_format(fmt, seconds);
	}
    string Date::format(const std::string & fmt, const Date & date) {
        // TODO: real formatter
        char buffer[1024] = {0,};
        snprintf(buffer, sizeof(buffer), "%04d-%02d-%02d %02d:%02d:%02d", date.getYear(), date.getMonth(), date.getDay(), date.getHour(), date.getMinute(), date.getSecond());
        return buffer;
    }
	void Date::setYear(int year) {
		this->year = year;
	}
	void Date::setMonth(int month) {
		this->month = month;
	}
	void Date::setDay(int day) {
		this->day = day;
	}
	void Date::setHour(int hour) {
		this->hour = hour;
	}
	void Date::setMinute(int minute) {
		this->minute = minute;
	}
	void Date::setSecond(int second) {
		this->second = second;
	}
	void Date::setMillisecond(int millisecond) {
		this->millisecond = millisecond;
	}
	int Date::getYear() const {
		return year;
	}
	int Date::getMonth() const {
		return month;
	}
	int Date::getDay() const {
		return day;
	}
	int Date::getHour() const {
		return hour;
	}
	int Date::getMinute() const {
		return minute;
	}
	int Date::getSecond() const {
		return second;
	}
	int Date::getMillisecond() const {
		return millisecond;
	}

	// file system
#if defined(USE_UNIX_STD)
    
    static bool s_is_separator(char c, const string & separators);

    static string s_get_separators() {
        return "/";
    }
    static bool s_is_separator(char c) {
        return s_is_separator(c, s_get_separators());
    }
    static bool s_is_separator(char c, const string & separators) {
        for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
            char sep = *iter;
            if (sep == c) {
                return true;
            }
        }
        return false;
    }
    static string s_append_separator_if_not(const string & path, const string & separators) {
        
        if (path.empty() || separators.empty()) {
            return path;
        }
        
        char end = *(path.rbegin());
        for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
            char sep = *iter;
            if (sep == end) {
                return path;
            }
        }
        
        return path + *(separators.begin());
    }
    static string s_remove_last_separator(const string & path) {
        if (!path.empty() && path.length() > 1 && s_is_separator(*(path.rbegin())) ) {
            return path.substr(0, path.length() - 1); // trailing last / character
        }
        return path;
    }
	static bool s_is_fullpath(const string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
    static string s_get_cwd() {
        char buffer[2048] = {0,};
        if (getcwd(buffer, sizeof(buffer)) == NULL) {
            throw IOException("getcwd() error", -1, 0);
        }
        return string(buffer);
    }
	static bool s_is_root_path(const string & path) {
		return !path.compare("/");
	}
	static bool s_exists(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c
		struct stat st;
		return (stat(path.c_str(), &st) == 0);
	}
	static bool s_is_file(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/3536781/accessing-directories-in-c/3536853#3536853
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? false : true);
	}
	static bool s_is_directory(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? true : false);
	}
	static bool s_is_writable(const string & path) {
		return (access(path.c_str(), W_OK) == 0);
	}
	static string s_get_parent_path(const string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

        string p = s_remove_last_separator(path);
		size_t f = p.find_last_of(s_get_separators());
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(const string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_last_separator(path);
		}

		size_t f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(const string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		size_t f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(const string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
            string p = s_remove_last_separator(path);
			size_t f = p.find_last_of(s_get_separators());
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(const string & path) {
		string name = s_get_entity_name_part(path);
		size_t f = name.find_last_of(".");
		if (f == string::npos || f == 0) {
			return "";
		}
		return name.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, mode_t mode) {
	
		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(tmp[len - 1] == '/') {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(*p == '/') {
				*p = 0;
				mkdir(tmp, mode); // ignore error (just try)
				*p = '/';
			}
		}
	
		return mkdir(tmp, mode);
	}

	static TIME s_get_creation_date(const string & path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return st.st_ctime;
	}

	static TIME s_get_modified_date(const string & path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return st.st_mtime;
	}
    
    static filesize_t s_get_file_size(const string & path) {
        
        struct stat st;
        lstat(path.c_str(), &st);
        
        return st.st_size;
    }

	static std::vector<File> s_list(const string & path) {
		std::vector<File> ret;
		struct dirent * ent = NULL;
		struct dirent ** list = NULL;
		int cnt;
		cnt = scandir(path.c_str(), &list, NULL, alphasort);
		if (cnt < 0) {
			return ret;
		}
		for (int i = 0; i < cnt; i++) {
			ent = list[i];
			ret.push_back(File::fullpath(path, ent->d_name));
			free(ent);
		}
		free(list);
		return ret;
	}
	
#elif defined(USE_MS_WIN)

#define STAT_STRUCT struct _stat64
#define STAT_FUNC __stat64

	static string s_get_separators();
	static bool s_is_separator(char c);
	static bool s_is_separator(char c, const string & separators);
	static string s_remove_if_last(const string & path, char m);
	static bool s_is_fullpath(const string & path);
	static bool s_is_root_path(const string & path);
	static bool s_exists(const string & path);
	static bool s_is_file(const string & path);
	static bool s_is_directory(const string & path);
	static string s_get_parent_path(const string & path);
	static string s_get_path_part(const string & path);
	static string s_get_filename_part(const string & path);
	static string s_get_entity_name_part(const string & path);
	static string s_get_ext(const string & path);
	static int s_mkdir(const char *dir, int mode);
	static TIME s_get_creation_date(const string & path);
	static TIME s_get_modified_date(const string & path);

	static string s_get_separators() {
		return "\\/";
	}
	static bool s_is_separator(char c) {
		return s_is_separator(c, s_get_separators());
	}
	static bool s_is_separator(char c, const string & separators) {
		for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
			char sep = *iter;
			if (sep == c) {
				return true;
			}
		}
		return false;
	}
	static char s_get_default_seprator() {
		return *(s_get_separators().begin());
	}
	static string s_get_default_seprator_in_string() {
		return string(1, s_get_default_seprator());
	}
	static string s_append_separator_if_not(const string & path) {
		if (!s_is_separator(*(path.rbegin()))) {
			return path + s_get_default_seprator();
		}
		return path;
	}
	static string s_append_separator_if_not(const string & path, const string & separators) {

		if (path.empty() || separators.empty()) {
			return path;
		}

		char end = *(path.rbegin());
		for (string::const_iterator iter = separators.begin(); iter != separators.end(); iter++) {
			char sep = *iter;
			if (sep == end) {
				return path;
			}
		}

		return path + *(separators.begin());
	}
	static string s_remove_last_separator(const string & path) {
		if (!path.empty() && path.length() > 1 && s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(const string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}

	static string s_get_cwd() {
		char buffer[2048] = {0,};
		if (_getcwd(buffer, sizeof(buffer)) == NULL) {
			throw IOException("_getcwd() error", -1, 0);
		}
		return string(buffer);
	}

	static bool s_is_root_path(const string & path) {
		return (path.length() == 1 && s_is_separator(*path.begin()));
	}
	static bool s_exists(const string & path) {

		if (path.empty()) {
			return false;
		}

		if (s_is_directory(path) || s_is_file(path)) {
			return true;
		}
		
		return false;
	}
	static bool s_is_file(const string & path) {

		if (path.empty()) {
			return false;
		}

		STAT_STRUCT s;
		if (STAT_FUNC(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFREG ? true : false);
	}
	static bool s_is_directory(const string & path) {

		if (path.empty()) {
			return false;
		}
		
		STAT_STRUCT s;
		if (STAT_FUNC(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFDIR ? true : false);
	}
	static bool s_is_writable(const string & path) {
		if (!s_exists(path)) {
			return false;
		}
		return (_access(path.c_str(), 2) == 0);
	}
	static string s_get_parent_path(const string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

		string p = s_remove_last_separator(path);
		int f = p.find_last_of(s_get_separators());
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(const string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_last_separator(path);
		}

		int f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(const string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		int f = path.find_last_of(s_get_separators());
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(const string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
			string p = s_remove_last_separator(path);
			int f = p.find_last_of(s_get_separators());
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(const string & path) {
		string name = s_get_entity_name_part(path);
		size_t f = name.find_last_of(".");
		if (f == string::npos || f == 0) {
			return "";
		}
		return name.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, int mode) {
	
		// https://msdn.microsoft.com/en-us/library/2fkk4dzw.aspx

		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(s_is_separator(tmp[len - 1])) {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(s_is_separator(*p)) {
				*p = 0;
				_mkdir(tmp);
				*p = s_get_default_seprator();
			}
		}
	
		return _mkdir(tmp);


		return 0;
	}
	static SYSTEMTIME s_filetime_to_systemtime(FILETIME ftime) {
		SYSTEMTIME stime;
		FileTimeToSystemTime(&ftime, &stime);
		return stime;
	}
	static TIME s_get_creation_date(const string & path) {

		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return s_filetime_to_systemtime(ftCreate);
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return s_filetime_to_systemtime(ftCreate);
		}

		return s_filetime_to_systemtime(ftCreate);
	}
	static TIME s_get_modified_date(const string & path) {
		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return s_filetime_to_systemtime(ftWrite);
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return s_filetime_to_systemtime(ftWrite);
		}

		return s_filetime_to_systemtime(ftWrite);
	}

	static filesize_t s_get_file_size(const string & path) {

		STAT_STRUCT st;
		int ret = STAT_FUNC(path.c_str(), &st);
		if (ret != 0) {
			// error
			throw Exception("stat() failed", -1, 0);
		}

		return (filesize_t)st.st_size;
	}

	static std::vector<File> s_list(const string & path) {

		std::vector<File> ret;

		string dir = path;
		dir.append("\\*");

		WIN32_FIND_DATAA ffd;
		HANDLE hFind = INVALID_HANDLE_VALUE;
		DWORD dwError = 0;

		hFind = FindFirstFileA(dir.c_str(), &ffd);
		if (hFind == INVALID_HANDLE_VALUE) {
			throw IOException("FindFirstFileA() failed", -1, 0);
		}

		do {
			
			ret.push_back(File::fullpath(path, ffd.cFileName));

		} while (FindNextFileA(hFind, &ffd) != 0);

		FindClose(hFind);

		return ret;
	}
	
#endif

	/**
	 * @brief file constructor
	 */
	File::File() {
	}
	
	File::File(const string & path) : path(path) {
	}

	File::~File() {
	}

	string File::mergePaths(const string & dir, const string & filename) {
		return fullpath(dir, filename);
	}

	string File::mergePaths(const string & dir, const string & filename, const string & separators) {
		return fullpath(dir, filename, separators);
	}

	string File::fullpath(const string & dir, const string & filename) {
		return fullpath(dir, filename, s_get_separators());
	}

	string File::fullpath(const string & dir, const string & filename, const string & separators) {
		
		string d = dir;
		string fname = filename;

		d = s_append_separator_if_not(d, separators);

		if (!fname.empty()) {
			size_t f = fname.find_first_not_of(separators);
			fname = (f != string::npos ? fname.substr(f) : "");
		}

		return (d + fname);
	}

	string File::getCwd() {
		return s_get_cwd();
	}

	bool File::isRootPath(const string & path){
		return s_is_root_path(path);
	}

	bool File::isFullpath(const string & path) {
		return s_is_fullpath(path);
	}
	
	bool File::exists(const string & path){
		return s_exists(path);
	}

	bool File::isFile(const string & path){
		return s_is_file(path);
	}

	bool File::isDirectory(const string & path){
		return s_is_directory(path);
	}

	bool File::isWritable(const string & path) {
		return s_is_writable(path);
	}

	string File::getParentPath(const string & path) {
		return s_get_parent_path(path);
	}

	string File::getPathPart(const string & path){
		return s_get_path_part(path);
	}

	string File::getFileNamePart(const string & path){
		return s_get_filename_part(path);
	}

	string File::getExtension(const string & path){
		return s_get_ext(path);
	}

	string File::getEntityNamePart(const string & path) {
		return s_get_entity_name_part(path);
	}

	bool File::compareExtension(const string & path, string extension){
		
		string a = s_get_ext(path);
		string b = s_get_ext(extension);

		if (a.empty() || b.empty()) {
			return false;
		}

		return (!a.compare(b));
	}

	int File::mkdir(const string & path) {
		return s_mkdir(path.c_str(), 0755);
	}

	string File::getCreationDate(const string & path, string fmt) {
		TIME t = s_get_creation_date(path);
		return Date::format(fmt, t);
	}
	string File::getModifiedDate(const string & path, string fmt) {
		TIME t = s_get_modified_date(path);
		return Date::format(fmt, t);
	}
	filesize_t File::getSize(const string & path) {
		return s_get_file_size(path);
	}
	vector<File> File::list(const string & path) {
		return s_list(path);
	}
	string File::getName() {
		return getEntityNamePart(path);
	}
	string File::toString() {
		return path;
	}
	std::string File::getPath() const {
		return path;
	}
	bool File::isRootPath() {
		return File::isRootPath(path);
	}
	bool File::isFullpath() {
		return File::isFullpath(path);
	}
	bool File::exists() {
		return File::exists(path);
	}
	bool File::isFile() {
		return File::isFile(path);
	}
	bool File::isDirectory() {
		return File::isDirectory(path);
	}
	bool File::isWritable() {
		return File::isWritable(path);
	}
	string File::getParentPath() {
		return File::getParentPath(path);
	}
	string File::getPathPart() {
		return File::getPathPart(path);
	}
	string File::getFileNamePart() {
		return File::getFileNamePart(path);
	}
	string File::getExtension() {
		return File::getExtension(path);
	}
	string File::getEntityNamePart() {
		return File::getEntityNamePart(path);
	}
	bool File::compareExtension(string extension) {
		return File::compareExtension(path, extension);
	}
	int File::mkdir() {
		return File::mkdir(path);
	}
	string File::getCreationDate(const string & fmt) {
		return File::getCreationDate(path, fmt);
	}
	string File::getModifiedDate(const string & fmt) {
		return File::getModifiedDate(path, fmt);
	}
	filesize_t File::getSize() {
		return File::getSize(path);
	}
	vector<File> File::list() {
		return File::list(path);
	}
	
} /* OS */
