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
		throw NotImplementedException("Not implemented");
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
		throw NotImplementedException("Not implemented");
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
		SUPPRESS_UNUSED(handle);
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
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
	
		pthread_attr_t attr;
		bool started = false;

		if (!handle || !thread) {
			return false;
		}
		if (pthread_attr_init(&attr) != 0) {
			return false;
		}

		try {
			if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0) {
				throw Exception("pthread_attr_setdetachstate() failed");
			}
			if (stack_size > 0) {
				if (pthread_attr_setstacksize(&attr, stack_size) != 0) {
					throw Exception("pthread_attr_setstacksize() failed");
				}
			}
			if (pthread_create(handle, &attr, func, (void*)thread) != 0) {
				throw Exception("pthread_create() failed");
			}
			started = true;
			
		} catch (Exception e) {
			// ...
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
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
		UINT dwThreadID;
		HANDLE h;

		if (!handle || !thread) {
			return false;
		}

		h = (HANDLE)_beginthreadex(NULL, stack_size, func, (void*)thread, 0, &dwThreadID);
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
	Thread::Thread() : handle(0), signal_interrupt(false), stack_size(0) {
		init();
	}

	Thread::Thread(size_t stack_size) : handle(0), signal_interrupt(false), stack_size(stack_size) {
		init();
	}
	
	Thread::~Thread() {
	}

	void Thread::init() {
		id = (++s_thread_id_seed == 0) ? ++s_thread_id_seed : s_thread_id_seed;

		reset();
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
			bool ret = s_startThread(&handle, s_thread_wrapper, this, stack_size);
			running = true;
			return ret;
		}
		return false;
	}
	
	void Thread::interrupt() {
		signal_interrupt = true;
		onInterrupt();
	}

	bool Thread::interrupted() {
		bool ret = signal_interrupt;
		signal_interrupt = false;
		return ret;
	}

	bool Thread::isRunning() {
		return running;
	}

	void Thread::onInterrupt() {
		/* virtual */
	}

	void Thread::wait() {
		while (running) { idle(10); }
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
	
	void InetAddress::setAddress(const string & host, int port) {
		this->host = host;
		this->port = port;
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
		memset(mac_address, 0, 6);
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
	void NetworkInterface::setMacAddress(const unsigned char * mac_address, size_t size) {
		size_t s = sizeof(this->mac_address);
		memcpy(this->mac_address, mac_address, (s < size ? s : size));
	}
	void NetworkInterface::getMacAddress(unsigned char * out, size_t size) const {
		size_t s = sizeof(this->mac_address);
		memcpy(out, this->mac_address, (s < size ? s : size));
	}
	void NetworkInterface::setLoopBack(bool loopback) {
		this->loopback = loopback;
	}
    bool NetworkInterface::isLoopBack() const {
		if (loopback) {
			return true;
		}
		for (vector<InetAddress>::const_iterator iter = inetAddresses.begin(); iter != inetAddresses.end(); iter++) {
			if (!iter->getHost().compare("127.0.0.1") ||
                !iter->getHost().compare("::1")) {
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
            
            if (tmp->ifa_addr) {

				NetworkInterface & iface = s_obtain_network_interface(ifaces, tmp->ifa_name);

				switch (tmp->ifa_addr->sa_family) {
				case AF_INET:
					{
						iface.setInetAddress(InetAddress((struct sockaddr*)tmp->ifa_addr));
					}
					break;
				case AF_INET6:
					{
						iface.setInetAddress(InetAddress((struct sockaddr*)tmp->ifa_addr));
					}
					break;
#if defined(USE_APPLE_STD)
				case AF_LINK:
					{
						unsigned char * ptr = (unsigned char *)LLADDR((struct sockaddr_dl *)(tmp->ifa_addr));
						iface.setMacAddress(ptr, 6);
					}
					break;
#else
				case AF_PACKET:
					{
						struct sockaddr_ll * s = (struct sockaddr_ll*)tmp->ifa_addr;
						iface.setMacAddress(s->sll_addr, 6);
					}
					break;
#endif
				default:
					break;
				}
                
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

	Selectable::Selectable() {
	}
	
	Selectable::~Selectable() {
	}
    
    // bool Selectable::isSelectable() {
    //     return selectable;
    // }
    // void Selectable::setSelectable(bool selectable) {
    //     this->selectable = selectable;
    // }
	void Selectable::registerSelector(Selector & selector, unsigned char flags) {
		selector.set(getFd(), flags);
	}
	
	void Selectable::unregisterSelector(Selector & selector, unsigned char flags) {
		selector.unset(getFd(), flags);
	}
	
	bool Selectable::isReadable(Selector & selector) {
		return selector.isReadable(getFd());
	}

	bool Selectable::isWritable(Selector & selector) {
		return selector.isWritable(getFd());
	}
	
	bool Selectable::isExcept(Selector & selector) {
		return selector.isExcept(getFd());
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
    
    bool Selector::isRegistered(int fd, unsigned char type) {
        switch (type) {
        case READ:
            return FD_ISSET(fd, &readfds) ? true : false;
        case WRITE:
            return FD_ISSET(fd, &writefds) ? true : false;
        case EXCEPT:
            return FD_ISSET(fd, &exceptfds) ? true : false;
        default:
            return false;
        }
    }
    
    bool Selector::isReadable(int fd) {
        return FD_ISSET(fd, &curreadfds) ? true : false;
    }
    
    bool Selector::isReadable(Selectable & selectable) {
        return isReadable(selectable.getFd());
    }
    
    bool Selector::isWritable(int fd) {
        return FD_ISSET(fd, &curwritefds) ? true : false;
    }
    
    bool Selector::isWritable(Selectable & selectable) {
        return isWritable(selectable.getFd());
    }

	bool Selector::isExcept(int fd) {
        return FD_ISSET(fd, &curexceptfds) ? true : false;
    }
    
    bool Selector::isExcept(Selectable & selectable) {
        return isExcept(selectable.getFd());
    }



	SharedSelector::SharedSelector() : semSet(1), semCur(1) {
	}
	SharedSelector::~SharedSelector() {
	}
	void SharedSelector::set(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::set(fd, flags);
	}
	void SharedSelector::unset(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::unset(fd, flags);
	}
	int SharedSelector::select(unsigned long timeout_milli) {
		AutoLock lock(semCur);
		return Selector::select(timeout_milli);
	}
	
	std::vector<Selection> & SharedSelector::getSelections() {
		AutoLock lock(semCur);
		return Selector::getSelections();
	}

	bool SharedSelector::isReadable(int fd) {
		AutoLock lock(semCur);
		return Selector::isReadable(fd);
	}
	bool SharedSelector::isReadable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isReadable(selectable);
	}
	bool SharedSelector::isWritable(int fd) {
		AutoLock lock(semCur);
		return Selector::isWritable(fd);
	}
	bool SharedSelector::isWritable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isWritable(selectable);
	}
	bool SharedSelector::isExcept(int fd) {
		AutoLock lock(semCur);
		return Selector::isExcept(fd);
	}
	bool SharedSelector::isExcept(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isExcept(selectable);
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

	void SocketUtil::throwSocketException(const string & message) {

#if defined(USE_BSD_SOCKET)
        
        int err = errno;
        // char text[1024] = {0,};
        // if (strerror_r(err, text, sizeof(text))) {}
		char * text = strerror(err);
        throw IOException(message + " / " + string(text), err, 0);
        
#elif defined(USE_WINSOCK2)
		int err = WSAGetLastError();
		char text[1024];
		FormatMessageA(FORMAT_MESSAGE_FROM_SYSTEM | FORMAT_MESSAGE_IGNORE_INSERTS |
						FORMAT_MESSAGE_MAX_WIDTH_MASK, NULL, err,
						MAKELANGID(LANG_NEUTRAL, SUBLANG_DEFAULT), (LPSTR)text, 1024, NULL);
		throw IOException(message + " / " + string(text), err, 0);
#else
		throw IOException("unknown socket exception", -1, 0);
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
            // throw IOException("setsockopt() error", -1, 0);
			throwSocketException("setsockopt() error");
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

	DatagramPacket::DatagramPacket(char * data, size_t size)
		: data(data), size(size), position(0), length(0) {
        System::getInstance();
	}

	DatagramPacket::DatagramPacket(char * data, size_t size, const InetAddress & remoteAddr)
		: data(data), size(size), position(0), length(0), remoteAddr(remoteAddr) {
		System::getInstance();
	}
	
	DatagramPacket::DatagramPacket(char * data, size_t size, const string & host, int port)
		: data(data), size(size), position(0), length(0), remoteAddr(host, port) {
		System::getInstance();
	}

	DatagramPacket::~DatagramPacket() {
	}

	void DatagramPacket::clear() {
		memset(data, 0, size);
		position = 0;
		length = 0;
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
	
	void DatagramPacket::write(const unsigned char i8) {
		write((const char *)&i8, sizeof(unsigned char));
	}
	
	void DatagramPacket::write(const unsigned short i16) {
		write((const char *)&i16, sizeof(unsigned short));
	}
	
	void DatagramPacket::write(const unsigned int i32) {
		write((const char *)&i32, sizeof(unsigned int));
	}
	
	void DatagramPacket::write(const unsigned long long i64) {
		write((const char *)&i64, sizeof(unsigned long long));
	}
	
	void DatagramPacket::write(const char * data, size_t size) {
		if (this->size < position + size) {
			throw BufferOverflowException("buffer overflowed", -1, 0);
		}
		memcpy(this->data + position, data, size);
		position += size;
		setLength(position + size);
	}
	
	void DatagramPacket::write(const string & data) {
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

	void DatagramPacket::setRemoteAddr(const string & host, int port) {
		this->remoteAddr.setAddress(host, port);
	}
	
} /* OS */
