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
		usleep((useconds_t)timeout * 1000);
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
#if defined(USE_UNIX_STD)

		struct timeval tv;
		if( gettimeofday(&tv, NULL) != 0 )
			return 0;
		
		return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
		
#elif defined(USE_MS_WIN)
		return GetTickCount();
#else
		return 0;
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
    };
    
#if defined(USE_UNIX_STD)
    class NixSystemImpl : public SystemImpl {
    public:
        NixSystemImpl() {
        }
        virtual ~NixSystemImpl() {
        }
    };
    static NixSystemImpl s_systemImpl;
#elif defined(USE_MS_WIN)
    class MSSystemImpl : public SystemImpl {
    public:
        MSSystemImpl() {
            WSADATA wsaData;
            WSAStartup(MAKEWORD(2,2), &wsaData);
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
	
	void Semaphore::wait() {
		s_sem_wait(&handle);
	}
	
	void Semaphore::post() {
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


	/* Network*/
	
#if defined(USE_BSD_SOCKET)

	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
	
		struct ifaddrs * ifAddrStruct = NULL;
		struct ifaddrs * ifa = NULL;
		void * tmpAddrPtr = NULL;
		getifaddrs( &ifAddrStruct );
		int found=0;

		for (ifa = ifAddrStruct; ifa != NULL; ifa = ifa->ifa_next) {
			if ( ifa->ifa_addr != NULL && ifa ->ifa_addr->sa_family == AF_INET ) { // check it is IP4
				// is a valid IP4 Address
				tmpAddrPtr=&((struct sockaddr_in *)ifa->ifa_addr)->sin_addr;
				inet_ntop( AF_INET, tmpAddrPtr, ipAddressBuffer, INET_ADDRSTRLEN );
				if ( strcmp( ifa->ifa_name, ifname )==0 ) {
					found = 1;
					break;
				}
			}
		}
		if (ifAddrStruct!=NULL) freeifaddrs(ifAddrStruct);
		return found;
	}

#elif defined(USE_WINSOCK2)

#if 0
	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
		PIP_INTERFACE_INFO pInfo = NULL;
		ULONG ulOutBufLen = 0;

		DWORD dwRetVal = 0;
		int iReturn = 1;

		int i;

		dwRetVal = GetInterfaceInfo(NULL, &ulOutBufLen);
		if (dwRetVal == ERROR_INSUFFICIENT_BUFFER) {
			pInfo = (IP_INTERFACE_INFO *) MALLOC(ulOutBufLen);
			if (pInfo == NULL) {
				printf("Unable to allocate memory needed to call GetInterfaceInfo\n");
				return 1;
			}
		}

		dwRetVal = GetInterfaceInfo(pInfo, &ulOutBufLen);
		if (dwRetVal == NO_ERROR) {
			printf("Number of Adapters: %ld\n\n", pInfo->NumAdapters);
			for (i = 0; i < pInfo->NumAdapters; i++) {
	            printf("Adapter Index[%d]: %ld\n", i, pInfo->Adapter[i].Index);
				printf("Adapter Name[%d]: %ws\n\n", i, pInfo->Adapter[i].Name);
			}
			iReturn = 0;
		} else if (dwRetVal == ERROR_NO_DATA) {
	        printf("There are no network adapters with IPv4 enabled on the local system\n");
			iReturn = 0;
		} else {
	        printf("GetInterfaceInfo failed with error: %d\n", dwRetVal);
			iReturn = 1;
		}

		FREE(pInfo);

		return iReturn;
	}
#endif

	/**
	 * @ref http://stackoverflow.com/questions/1673931/how-do-i-enumerate-network-adapters-and-get-their-mac-addresses-in-win32-api-c
	 * @ref https://msdn.microsoft.com/en-us/library/windows/desktop/aa365917%28v=vs.85%29.aspx
	 */
	static int s_get_ip_address(const char * ifname, char * ipAddressBuffer) {
		ULONG outBufLen = 0;
		DWORD dwRetVal = 0;
		IP_ADAPTER_INFO* pAdapterInfos = (IP_ADAPTER_INFO*) malloc(sizeof(IP_ADAPTER_INFO));

		// retry up to 5 times, to get the adapter infos needed
		for( int i = 0; i < 5 && (dwRetVal == ERROR_BUFFER_OVERFLOW || dwRetVal == NO_ERROR); ++i )
		{
			dwRetVal = GetAdaptersInfo(pAdapterInfos, &outBufLen);
			if( dwRetVal == NO_ERROR )
			{
				break;
			}
			else if( dwRetVal == ERROR_BUFFER_OVERFLOW )
			{
				free(pAdapterInfos);
				pAdapterInfos = (IP_ADAPTER_INFO*) malloc(outBufLen);
			}
			else
			{
				pAdapterInfos = 0;
				break;
			}
		}
		if( dwRetVal == NO_ERROR )
		{
			IP_ADAPTER_INFO* pAdapterInfo = pAdapterInfos;
			while( pAdapterInfo )
			{
				IP_ADDR_STRING* pIpAddress = &(pAdapterInfo->IpAddressList);
				while( pIpAddress != 0 )
				{
					// 
					// <<<<
					// here pAdapterInfo->Address should contain the MAC address
					// >>>>
					// 

					printf("[%s] ip address: %s\n", pAdapterInfo->AdapterName, pAdapterInfo->IpAddressList.IpAddress.String);

					pIpAddress = pIpAddress->Next;
				}
				pAdapterInfo = pAdapterInfo->Next;
			}
		}
		free(pAdapterInfos);
		return false;
	}

#endif
		
	string Network::getIPAddress(const string & iface) {
		return getIPAddress(iface.c_str());
	}

	string Network::getIPAddress(const char * iface) {
		char ipaddr[1024] = {0,};
		if (s_get_ip_address(iface, ipaddr) > 0) {
			return string(ipaddr);
		}

		return "0.0.0.0";
	}


	/* select */
	Selector::Selector() : maxfds(0) {
		FD_ZERO(&readfds);
		FD_ZERO(&curfds);
	}
	
	Selector::~Selector() {
	}
	void Selector::set(int fd) {
		if (fd > maxfds) {
			maxfds = fd;
		}
		FD_SET(fd, &readfds);
	}
	void Selector::unset(int fd) {
		FD_CLR(fd, &readfds);
	}
	int Selector::select(unsigned long timeout_milli) {

		struct timeval timeout;
		
		curfds = readfds;
		timeout.tv_sec = timeout_milli / 1000;
		timeout.tv_usec = (timeout_milli % 1000) * 1000;
		
		return ::select(maxfds + 1, &curfds, NULL, NULL, &timeout);
	}
	vector<int> & Selector::getSelected() {
		selected.clear();
		for (int i = 0; i < maxfds + 1; i++) {
			if (FD_ISSET(i, &curfds)) {
				selected.push_back(i);
			}
		}
		return selected;
	}

	bool Selector::isSelected(int fd) {
		return FD_ISSET(fd, &curfds) ? true : false;
	}


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
	

	/* SOCKET implementation */
	
#if defined(USE_BSD_SOCKET)

	/**
	 * @brief BSD socket
	 */
	class BsdSocket : public Socket {
	private:
	public:
		BsdSocket(SOCK_HANDLE sock) : Socket(sock) {
		}
		BsdSocket(const char * host, int port) {
			setAddress(host, port);
		}
		virtual ~BsdSocket() {
		}
		virtual int connect() {
            
			int ret = 0;
			struct addrinfo hints, * res = NULL;
			char port[10] = {0,};

			SOCK_HANDLE sock = -1;
			socket(-1);
			snprintf(port, sizeof(port), "%d", getPort());
            
            ScopedConnection scopedConnection(&res);

			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_STREAM;
			if (::getaddrinfo(getHost(), port, &hints, &res) < 0) {
                throw IOException("getaddrinfo() error", -1, 0);
			}

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock < 0) {
                throw IOException("socket() error", -1, 0);
			}

			if (::connect(sock, res->ai_addr, res->ai_addrlen) < 0) {
				::close(sock);
                throw IOException("connect() error", -1, 0);
			}

			socket(sock);
			
			return ret;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set(socket());
		}
		virtual bool compareFd(int fd) {
			return socket() == fd;
		}
		virtual int getFd() {
			return socket();
		}
		virtual int recv(char * buffer, size_t max) {
            ssize_t ret = ::read(socket(), buffer, max);
            if (ret <= 0) {
                throw IOException("read() error", (int)ret, 0);
            }
			return (int)ret;
		}
		virtual int send(const char * buffer, size_t length) {
            ssize_t ret = ::write(socket(), buffer, length);
            if (ret <= 0) {
                throw IOException("write() error", (int)ret, 0);
            }
			return (int)ret;
		}
		virtual void shutdown(/* type */) {}
		virtual void close() {
			::close(socket());
		}
	};

#elif defined(USE_WINSOCK2)

	/*
	 * Winsock2Socket
	 * reference
	 * - winsock: http://www.joinc.co.kr/modules/moniwiki/wiki.php/Site/win_network_prog/doc/winsock_basic
	 * - MSDN: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms737889%28v=vs.85%29.aspx
	 */
	class Winsock2Socket : public Socket {
	private:
		struct addrinfo * targetAddr;

	private:
		
	public:
		Winsock2Socket(SOCKET sock) : Socket(sock), targetAddr(NULL) {
		}
		Winsock2Socket(const char * host, int port) : targetAddr(NULL) {
			
			struct addrinfo hints;
			
			char portStr[10] = {0,};

			this->socket(INVALID_SOCKET);
			setAddress(host, port);

			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;

			snprintf(portStr, sizeof(portStr), "%d", port);
			if (getaddrinfo(host, portStr, &hints, &targetAddr) != 0) {
				// error
				targetAddr = NULL;
				throw IOException("getaddrinfo() error", -1, 0);
			}
		}

		virtual ~Winsock2Socket() {
			if (targetAddr) {
				freeaddrinfo(targetAddr);
			}
		}

		virtual int connect() {

			struct addrinfo * addr = NULL;
			int ret;
			SOCK_HANDLE sock;

			if (!targetAddr) {
				// error
				throw IOException("no target address error", -1, 0);
			}

			for (addr = targetAddr; addr; addr = addr->ai_next) {
				// try to connect until one succeeds
				sock = ::socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
				if (sock == INVALID_SOCKET) {
					// error
					freeaddrinfo(targetAddr);
					throw IOException("socket() error", -1, 0);
				}

				ret = ::connect(sock, addr->ai_addr, (int)addr->ai_addrlen);
				if (ret == SOCKET_ERROR) {
					// error
					::closesocket(sock);
					sock = INVALID_SOCKET;
					continue;
				}
				break;
			}

			freeaddrinfo(targetAddr);
			targetAddr = NULL;

			if (sock == INVALID_SOCKET) {
				throw IOException("invalid socket error", -1, 0);
			}

			this->socket(sock);

			return 0;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set((int)this->socket());
		}
		virtual bool compareFd(int fd) {
			return (int)this->socket() == fd;
		}
		virtual int getFd() {
			return (int)this->socket();
		}
		virtual int recv(char * buffer, size_t max) {
			if (this->socket() == INVALID_SOCKET) {
				throw IOException("invalid socket error", -1, 0);
			}
            int ret = ::recv(this->socket(), buffer, max, 0);
            
            if (ret <= 0) {
                throw IOException("recv() error", ret, 0);
            }
            
			return ret;
		}

		virtual int send(const char * buffer, size_t length) {
			if (this->socket() == INVALID_SOCKET) {
                throw IOException("invalid socket error", -1, 0);
			}
			return ::send(this->socket(), buffer, length, 0);
		}

		virtual void shutdown(/* type */) {
		}

		virtual void close() {
			if (this->socket() == INVALID_SOCKET) {
				return;
			}
			closesocket(this->socket());
		}
	};

#endif

/*
	 * Socket
	 */

	Socket::Socket() : sock(0), socketImpl(NULL), port(0) {
		init();
		setHost(NULL);
	}

	Socket::Socket(SOCK_HANDLE sock) {
		init();
		this->sock = sock;
	}
	
	Socket::Socket(const char * host, int port) {
		init();
		setAddress(host, port);

#if defined(USE_BSD_SOCKET)
		socketImpl = new BsdSocket(host, port);
#elif defined(USE_WINSOCK2)
		socketImpl = new Winsock2Socket(host, port);
#endif
	}

	Socket::~Socket() {
		if (socketImpl) {
			delete socketImpl;
		}
	}

	void Socket::init() {
        
        System::getInstance();
        
		sock = 0;
		socketImpl = NULL;
		port = 0;
		setHost(NULL);
	}

	void Socket::setAddress(const char * host, int port) {
		setHost(host);
		this->port = port;
	}
	
	void Socket::setHost(const char * host) {
		memset(this->host, 0, sizeof(this->host));
		if (host) {
			snprintf(this->host, sizeof(this->host), "%s", host);
		}
	}	

	int Socket::connect() {
        CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->connect();
	}

	void Socket::registerSelector(Selector & selector) {
        CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->registerSelector(selector);
	}
	bool Socket::compareFd(int fd) {
        CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->compareFd(fd);
	}
	int Socket::getFd() {
        CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->getFd();
	}
	int Socket::recv(char * buffer, size_t max) {
        CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->recv(buffer, max);
	}
	
	int Socket::send(const char * buffer, size_t length) {
        CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->send(buffer, length);
	}

	void Socket::shutdown(/* type */) {
        CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->shutdown();
	}
	
	void Socket::close() {
        CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->close();
	}

	char * Socket::getHost() {
		return host;
	}

	int Socket::getPort() {
		return port;
	}

	SOCK_HANDLE Socket::socket() {
		return sock;
	}

	void Socket::socket(SOCK_HANDLE sock) {
		this->sock = sock;
	}

	/* Server Socket implementation */
	
#if defined(USE_BSD_SOCKET)
	/*
	 * BsdServerSocket
	 * http://www.joinc.co.kr/modules/moniwiki/wiki.php/Site/Network_Programing/Documents/socket_beginning
	 */
	class BsdServerSocket : public ServerSocket{
	private:
	public:
		BsdServerSocket(int port) {

			SOCK_HANDLE sock;
			
			setPort(port);
			sock = ::socket(AF_INET, SOCK_STREAM, 0);
			if (sock < 0) {
                throw IOException("socket() error", -1, 0);
			}

			socket(sock);
		}
		
		virtual ~BsdServerSocket() {
			close();
		}

		virtual void setReuseAddr() {
			int status;
			int on = 1;
			status = ::setsockopt(socket(), SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on));
			if (status != 0) {
				::close(socket());
			}
		}

		virtual void registerSelector(Selector & selector) {
			selector.set(socket());
		}
		virtual bool compareFd(int fd) {
			return socket() == fd;
		}
		virtual int getFd() {
			return socket();
		}

		virtual int bind() {
            int ret;
			struct sockaddr_in addr;

			memset(&addr, 0, sizeof(addr));
			addr.sin_family = AF_INET;
			addr.sin_addr.s_addr = htonl(INADDR_ANY);
			addr.sin_port = htons(getPort());
			if ((ret = ::bind(socket(), (struct sockaddr*)&addr, sizeof(addr))) != 0) {
				close();
			}
			return ret;
		}
	
		virtual bool listen(int max) {
			if (::listen(socket(), max) != 0) {
				// error
				close();
				return false;
			}
			return true;
		}
	
		virtual Socket * accept() {
			struct sockaddr_in clientaddr;
			socklen_t clientaddr_len = 0;
			SOCK_HANDLE client = ::accept(socket(), (struct sockaddr*)&clientaddr, &clientaddr_len);
			if (client < 0) {
				return NULL;
			}

			return new BsdSocket(client);
		}

		virtual void close() {
			::close(socket());
		}
	};
#elif defined(USE_WINSOCK2)

	class Winsock2ServerSocket : public ServerSocket {
	private:
		struct addrinfo * addr;

	private:
        
	public:
		Winsock2ServerSocket(int port) : addr(NULL) {
			SOCK_HANDLE sock;
			struct addrinfo hints;
			char portStr[10] = {0,};

			setPort(port);

			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_INET;
			hints.ai_socktype = SOCK_STREAM;
			hints.ai_protocol = IPPROTO_TCP;
			hints.ai_flags = AI_PASSIVE;

			snprintf(portStr, sizeof(portStr), "%d", port);

			// Resolve the server address and port
			if (getaddrinfo(NULL, portStr, &hints, &addr) != 0) {
				// error
				return;
			}

			sock = ::socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
			if (sock == SOCKET_ERROR) {
				// error
				freeaddrinfo(addr);
				return;
			}

			socket(sock);
		}

		virtual ~Winsock2ServerSocket() {
			close();
		}

		virtual void setReuseAddr() {
			int status;
			int on = 1;
			status = ::setsockopt(socket(), SOL_SOCKET, SO_REUSEADDR,
								  (const char*)&on, sizeof(on));
			if (status != 0) {
				throw IOException("setsockopt() error", -1, 0);
			}
		}

		virtual void registerSelector(Selector & selector) {
			selector.set((int)socket());
		}
		virtual bool compareFd(int fd) {
			return (int)socket() == fd;
		}
		virtual int getFd() {
			return (int)socket();
		}

		virtual int bind() {

			int ret = ::bind(socket(), addr->ai_addr, (int)addr->ai_addrlen);
			if (ret == SOCKET_ERROR) {
				freeaddrinfo(addr);
				close();
                return ret;
			}

			freeaddrinfo(addr);

			return ret;
		}
	
		virtual bool listen(int max) {

			int ret = ::listen(this->socket(), max); // SOMAXCONN
			if (ret == SOCKET_ERROR) {
				close();
				return false;
			}

			return true;
		}
	
		virtual Socket * accept() {
			SOCK_HANDLE client = ::accept(socket(), (sockaddr*)NULL, (int*)NULL);
			if (client == INVALID_SOCKET) {
				// error
				return NULL;
			}

			return new Winsock2Socket(client);
		}

		virtual void close() {
			closesocket(socket());
		}
	};

#endif

	/*
	 * ServerSocket
	 */
	ServerSocket::ServerSocket() {
		init();
	}

	ServerSocket::ServerSocket(int port) {
		
		init();
		this->port = port;

#if defined(USE_BSD_SOCKET)
		serverSocketImpl = new BsdServerSocket(port);
#elif defined(USE_WINSOCK2)
		serverSocketImpl = new Winsock2ServerSocket(port);
#endif

	}

	ServerSocket::~ServerSocket() {
		if (serverSocketImpl) {
			delete serverSocketImpl;
		}
	}

	void ServerSocket::init() {
        
        System::getInstance();
        
		serverSocketImpl = NULL;
		port = 0;
	}

	void ServerSocket::setPort(int port) {
		this->port = port;
	}

	void ServerSocket::setReuseAddr() {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		serverSocketImpl->setReuseAddr();

	}

	void ServerSocket::registerSelector(Selector & selector) {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		serverSocketImpl->registerSelector(selector);
	}

	bool ServerSocket::compareFd(int fd) {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		return serverSocketImpl->compareFd(fd);
	}

	int ServerSocket::getFd() {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		return serverSocketImpl->getFd();
	}
	
	int ServerSocket::bind() {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		return serverSocketImpl->bind();
	}
    
    int ServerSocket::randomBind(RandomPortBinder & portBinder) {
        int ret = -1;
        portBinder.start();
        while (!portBinder.wantFinish()) {
            setPort(portBinder.getNextPort());
            ret = bind();
            if (ret >= 0) {
                break;
            }
        }
        return ret;
    }
	
	bool ServerSocket::listen(int max) {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		return serverSocketImpl->listen(max);
	}
	
	Socket * ServerSocket::accept() {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
		return serverSocketImpl->accept();
	}

	void ServerSocket::close() {
        CHECK_NOT_IMPL_THROW(serverSocketImpl);
        serverSocketImpl->close();
	}

	int ServerSocket::getPort() {
		return port;
	}

	SOCK_HANDLE ServerSocket::socket() {
		return sock;
	}

	void ServerSocket::socket(SOCK_HANDLE sock) {
		this->sock = sock;
	}




	/* Datagram Socket Implementation */
	
#if defined(USE_BSD_SOCKET)

	/**
	 * @brief BSD Datagram Socket
	 */
	class BsdDatagramSocket : public DatagramSocket {
	private:
	public:
		BsdDatagramSocket(int port) {
			setPort(port);
			SOCK_HANDLE sock = ::socket(AF_INET, SOCK_DGRAM, 0);
			if (sock < 0) {
				throw IOException("socket() error", -1, 0);
			}
			socket(sock);
		}
		BsdDatagramSocket(const char * host, int port) {
			setAddress(host, port);
			if (connect() < 0) {
				throw IOException("connect() error", -1, 0);
			}
		}
		virtual ~BsdDatagramSocket() {
		}
        virtual void checkValidSocket(SOCK_HANDLE sock) {
            if (sock < 0) {
                throw new IOException("invalid socket", -1, 0);
            }
        }
		virtual void setReuseAddr() {
			int status;
			int on = 1;
			status = ::setsockopt(socket(), SOL_SOCKET, SO_REUSEADDR,
								  (const char*)&on, sizeof(on));
			if (status != 0) {
				// error
				::close(socket());
                throw IOException("setsockopt() error", -1, 0);
			}
            
            // http://stackoverflow.com/a/4766262
#ifdef __APPLE__
            status = ::setsockopt(socket(), SOL_SOCKET, SO_REUSEPORT, &on, sizeof(on));
            if (status != 0) {
                // error
                ::close(socket());
                throw IOException("setsockopt() error", -1, 0);
            }
#endif
		}
		virtual void setBroadcast() {
			int broadcast = 1;
			setsockopt(socket(), SOL_SOCKET, SO_BROADCAST,
					   &broadcast, sizeof(broadcast));
		}
		virtual int bind() {
			
			struct sockaddr_in server_addr;

			SOCK_HANDLE sock = socket();
			
			memset(&server_addr, 0, sizeof(server_addr));
			server_addr.sin_family = AF_INET;
			server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
			server_addr.sin_port = htons(getPort());
            
			return ::bind(sock, (struct sockaddr*)&server_addr, sizeof(server_addr));
		}
		virtual int joinGroup(const char * group) {
			
			struct ip_mreq mreq;

			SOCK_HANDLE sock = socket();
			int ret = 0;

			if ((ret = bind()) < 0) {
				throw IOException("bind() error", -1, 0);
			}

			mreq.imr_multiaddr.s_addr = ::inet_addr(group);
			mreq.imr_interface.s_addr = htonl(INADDR_ANY);
			ret = ::setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, &mreq, sizeof(mreq));
			return ret;
		}
		virtual int connect() {

			int ret = 0;
			struct addrinfo hints, * res = NULL;
			char port[10] = {0,};

			SOCK_HANDLE sock = -1;
			socket(-1);
			snprintf(port, sizeof(port), "%d", getPort());

			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			if (::getaddrinfo(getHost(), port, &hints, &res) < 0) {
				ret = -1;
				goto exit;
			}

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock < 0) {
				ret = -1;
				goto exit;
			}

			if (::connect(sock, res->ai_addr, res->ai_addrlen) < 0) {
				::close(sock);
				ret = -1;
				goto exit;
			}

			socket(sock);

		exit:
			if (res) {
				freeaddrinfo(res);
				res = NULL;
			}
			
			return ret;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set(socket());
		}
		virtual bool compareFd(int fd) {
			return socket() == fd;
		}
		virtual int getFd() {
			return socket();
		}
		virtual int recv(DatagramPacket & packet) {
			struct sockaddr_in client_addr;
			socklen_t client_addr_size = sizeof(client_addr);
			int ret = (int)::recvfrom(socket(), packet.getData(), packet.getMaxSize(), 0, 
				(struct sockaddr*)&client_addr, &client_addr_size);

			if (ret > 0) {
				packet.setLength(ret);
				packet.setRemoteAddr(getRemoteIPAddress(&client_addr));
				packet.setRemotePort(getRemotePortNumber(&client_addr));
			}
			
			return ret;
		}
		virtual int recv(char * buffer, size_t max) {
			struct sockaddr_in client_addr;
			socklen_t client_addr_size = sizeof(client_addr);
			return (int)::recvfrom(socket(), buffer, max, 0,
							  (struct sockaddr*)&client_addr, &client_addr_size);
		}
		virtual int send(const char * host,
						 int port,
						 const char * buffer,
						 size_t length) {

			int ret = -1;
			char portstr[10] = {0,};
			
			struct addrinfo hints, * res = NULL;
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			snprintf(portstr, sizeof(portstr), "%d", port);
			if (::getaddrinfo(host, portstr, &hints, &res) < 0) {
				return -1;
			}
			ret = (int)::sendto(socket(), buffer, length,
						   0, res->ai_addr, res->ai_addrlen);

			freeaddrinfo(res);
			
			return ret;
		}
		virtual void shutdown(/* type */) {}
		virtual void close() {
			::close(socket());
		}
	};

#elif defined(USE_WINSOCK2)

	/*
	 * Winsock2Socket
	 * reference
	 * - winsock: http://www.joinc.co.kr/modules/moniwiki/wiki.php/Site/win_network_prog/doc/winsock_basic
	 * - MSDN: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms737889%28v=vs.85%29.aspx
	 */
	class Winsock2DatagramSocket : public DatagramSocket {
	private:

	private:
		
	public:
		Winsock2DatagramSocket(int port) {
			setPort(port);

			SOCK_HANDLE sock = ::socket(AF_INET, SOCK_DGRAM, 0);
			if (sock == INVALID_SOCKET) {
				throw IOException("socket() error", -1, 0);
			}

			socket(sock);
		}
		Winsock2DatagramSocket(const char * host, int port) {
			setAddress(host, port);

			if (connect() < 0) {
				throw IOException("connect() error", -1, 0);
			}
		}

		virtual ~Winsock2DatagramSocket() {
		}

		virtual void checkValidSocket(SOCK_HANDLE sock) {
			if (sock == INVALID_SOCKET) {
				throw IOException("invalid socket", -1, 0);
			}
		}

		virtual void setReuseAddr() {
			int status;
			int on = 1;
			status = ::setsockopt(socket(), SOL_SOCKET, SO_REUSEADDR,
								  (const char*)&on, sizeof(on));
			if (status != 0) {
				throw IOException("setsockopt() error", -1, 0);
			}
		}
		virtual void setBroadcast() {
			int broadcast = 1;
			setsockopt(socket(), SOL_SOCKET, SO_BROADCAST,
					   (char *)&broadcast, sizeof(broadcast));
		}
		virtual int bind() {
			
			struct sockaddr_in server_addr;

			SOCK_HANDLE sock = socket();
			
			memset(&server_addr, 0, sizeof(server_addr));
			server_addr.sin_family = AF_INET;
			server_addr.sin_addr.s_addr = htonl(INADDR_ANY);
			server_addr.sin_port = htons(getPort());
			
			return ::bind(this->socket(),
						  (struct sockaddr*)&server_addr,
						  sizeof(server_addr));
		}
		virtual int joinGroup(const char * group) {
			
			struct ip_mreq mreq;

			SOCK_HANDLE sock = socket();
			int ret = 0;

			if (bind() < 0) {
				throw IOException("bind() error", -1, 0);
			}

			mreq.imr_multiaddr.s_addr = ::inet_addr(group);
			mreq.imr_interface.s_addr = htonl(INADDR_ANY);
			ret = ::setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, (const char*)&mreq, sizeof(mreq));
			return ret;
		}
		virtual int connect() {

			struct addrinfo hints;
			struct addrinfo * targetAddr = NULL;
			struct addrinfo * addr = NULL;
			int ret = 0;
			SOCK_HANDLE sock;

			char portStr[10] = {0,};
			
			socket(INVALID_SOCKET);

			ZeroMemory(&hints, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			hints.ai_protocol = IPPROTO_UDP;

			snprintf(portStr, sizeof(portStr), "%d", getPort());
			if (getaddrinfo(getHost(), portStr, &hints, &targetAddr) != 0) {
				targetAddr = NULL;
				ret = -1;
				goto exit;
			}


			for (addr = targetAddr; addr; addr = addr->ai_next) {
				// try to connect until one succeeds
				sock = ::socket(addr->ai_family, addr->ai_socktype, addr->ai_protocol);
				if (sock == INVALID_SOCKET) {
					// error
					freeaddrinfo(targetAddr);
					ret = -1;
					goto exit;
				}

				ret = ::connect(sock, addr->ai_addr, (int)addr->ai_addrlen);
				if (ret == SOCKET_ERROR) {
					// error
					::closesocket(sock);
					sock = INVALID_SOCKET;
					continue;
				}
				break;
			}

			freeaddrinfo(targetAddr);
			targetAddr = NULL;

			if (sock == INVALID_SOCKET) {
				ret = -1;
				goto exit;
			}

			this->socket(sock);

			exit:

			if (targetAddr) {
				freeaddrinfo(targetAddr);
			}

			return ret;
		}
		virtual void registerSelector(Selector & selector) {
			selector.set((int)socket());
		}
		virtual bool compareFd(int fd) {
			return (int)socket() == fd;
		}
		virtual int getFd() {
			return (int)socket();
		}
		virtual int recv(DatagramPacket & packet) {
			if (socket() == INVALID_SOCKET) {
				return -1;
			}
			
			struct sockaddr_in client_addr;
			socklen_t client_addr_size = sizeof(client_addr);
			int ret = (int)::recvfrom(socket(), packet.getData(), packet.getMaxSize(), 0, 
				(struct sockaddr*)&client_addr, &client_addr_size);

			if (ret > 0) {
				packet.setLength(ret);
				packet.setRemoteAddr(getRemoteIPAddress(&client_addr));
				packet.setRemotePort(getRemotePortNumber(&client_addr));
			}
			
			return ret;
		}
		virtual int recv(char * buffer, size_t max) {
			if (socket() == INVALID_SOCKET) {
				return -1;
			}
			
			struct sockaddr_in client_addr;
			socklen_t client_addr_size = sizeof(client_addr);
			int ret = (int)::recvfrom(socket(), buffer, max, 0, 
				(struct sockaddr*)&client_addr, &client_addr_size);

			return ret;
		}

		virtual int send(const char * host,
						 int port,
						 const char * buffer,
						 size_t length) {


			int ret = -1;
			char portstr[10] = {0,};
			
			struct addrinfo hints, * res = NULL;
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			snprintf(portstr, sizeof(portstr), "%d", port);
			if (::getaddrinfo(host, portstr, &hints, &res) < 0) {
				return -1;
			}
			ret = (int)::sendto(socket(), buffer, length,
				0, res->ai_addr, res->ai_addrlen);

			freeaddrinfo(res);

			return ret;
		}

		virtual void shutdown(/* type */) {
		}

		virtual void close() {
			if (socket() == INVALID_SOCKET) {
				return;
			}
			closesocket(socket());
		}
	};

#endif

	/*
	 * Datagram Packet
	 */

	DatagramPacket::DatagramPacket(char * data, size_t maxSize)
    : data(data), maxSize(maxSize), length(0), remotePort(0) {
        
        System::getInstance();
	}

	DatagramPacket::~DatagramPacket() {
	}

	char * DatagramPacket::getData() {
		return data;
	}

	size_t DatagramPacket::getLength() {
		return length;
	}

	size_t DatagramPacket::getMaxSize() {
		return maxSize;
	}

	void DatagramPacket::setLength(size_t length) {
		this->length = length;
	}

	string DatagramPacket::getRemoteAddr() {
		return remoteAddr;
	}

	int DatagramPacket::getRemotePort() {
		return remotePort;
	}

	void DatagramPacket::setRemoteAddr(string remoteAddr) {
		this->remoteAddr = remoteAddr;
	}

	void DatagramPacket::setRemotePort(int remotePort) {
		this->remotePort = remotePort;
	}


	/*
	 * Datagram Socket
	 */

	DatagramSocket::DatagramSocket() : sock(0), socketImpl(NULL), port(0) {
		init();
		setHost(NULL);
	}

	DatagramSocket::DatagramSocket(int port) {
		init();
		setPort(port);

#if defined(USE_BSD_SOCKET)
		socketImpl = new BsdDatagramSocket(port);
#elif defined(USE_WINSOCK2)
		socketImpl = new Winsock2DatagramSocket(port);
#endif
		
	}
	
	DatagramSocket::DatagramSocket(const char * host, int port) {
		init();
		setAddress(host, port);

#if defined(USE_BSD_SOCKET)
		socketImpl = new BsdDatagramSocket(host, port);
#elif defined(USE_WINSOCK2)
		socketImpl = new Winsock2DatagramSocket(host, port);
#endif
	}

	DatagramSocket::~DatagramSocket() {
		if (socketImpl) {
			delete socketImpl;
		}
	}

	void DatagramSocket::init() {

		System::getInstance();

		sock = 0;
		socketImpl = NULL;
		port = 0;
		setHost(NULL);
	}

	void DatagramSocket::setPort(int port) {
		setHost(NULL);
		this->port = port;
	}

	void DatagramSocket::setAddress(const char * host, int port) {
		setHost(host);
		this->port = port;
	}
	
	void DatagramSocket::setHost(const char * host) {
		memset(this->host, 0, sizeof(this->host));
		if (host) {
			snprintf(this->host, sizeof(this->host), "%s", host);
		}
	}

	void DatagramSocket::checkValidSocket(SOCK_HANDLE sock) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->checkValidSocket(sock);
	}

	void DatagramSocket::setReuseAddr() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->setReuseAddr();
	}
	void DatagramSocket::setBroadcast() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->setBroadcast();
	}
	int DatagramSocket::bind() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->bind();
	}
    int DatagramSocket::randomBind(RandomPortBinder & portBinder) {
        CHECK_NOT_IMPL_THROW(socketImpl);
        int ret = -1;
        portBinder.start();
        while (!portBinder.wantFinish()) {
            socketImpl->setPort(portBinder.getNextPort());
            ret = bind();
            if (ret >= 0) {
                break;
            }
        }
        return ret;
    }
	int DatagramSocket::joinGroup(const std::string & group) {
		return joinGroup(group.c_str());
	}
	int DatagramSocket::joinGroup(const char * host) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->joinGroup(host);
	}
	int DatagramSocket::connect() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->connect();
	}

	void DatagramSocket::registerSelector(Selector & selector) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->registerSelector(selector);
	}
	bool DatagramSocket::compareFd(int fd) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->compareFd(fd);
	}
	int DatagramSocket::getFd() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->getFd();
	}

	int DatagramSocket::recv(DatagramPacket & packet) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->recv(packet);
	}

	int DatagramSocket::recv(char * buffer, size_t max) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->recv(buffer, max);
	}
	
	int DatagramSocket::send(const char * host, int port,
							 const char * buffer, size_t length) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		return socketImpl->send(host, port, buffer, length);
	}

	void DatagramSocket::shutdown(/* type */) {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->shutdown();
	}
	
	void DatagramSocket::close() {
		CHECK_NOT_IMPL_THROW(socketImpl);
		socketImpl->close();
	}

	char * DatagramSocket::getHost() {
		return host;
	}

	int DatagramSocket::getPort() {
		return port;
	}

	SOCK_HANDLE DatagramSocket::socket() {
		checkValidSocket(sock);
		return sock;
	}

	void DatagramSocket::socket(SOCK_HANDLE sock) {
		this->sock = sock;
	}

	string DatagramSocket::getRemoteIPAddress(sockaddr_in * addr) {

		char ipstr[INET6_ADDRSTRLEN] = {0,};

#if (!defined(USE_WINSOCK2) || _WIN32_WINNT >= 0x0600)
		inet_ntop(addr->sin_family, (addr->sin_family == AF_INET ? 
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

	int DatagramSocket::getRemotePortNumber(sockaddr_in * addr) {
		return ntohs((addr->sin_family == AF_INET ? 
			((struct sockaddr_in*)addr)->sin_port: ((struct sockaddr_in6 *)addr)->sin6_port));
	}

	
	/* Date */
	
#if defined(USE_UNIX_STD)
	static string s_date_format(string fmt, TIME time) {
		char buffer[1024] = {0,};
		strftime(buffer, sizeof(buffer), fmt.c_str(), gmtime((time_t *)&time));

		return string(buffer);
	}
#elif defined(USE_MS_WIN)
	static string s_date_format(string fmt, TIME time) {
		char buffer[1024] = {0,};
		SYSTEMTIME stUTC;
		FileTimeToSystemTime(&time, &stUTC);
		snprintf(buffer, sizeof(buffer), "%d-%02d-%02d %02d:%02d:%02d", stUTC.wYear, stUTC.wMonth, stUTC.wDay, stUTC.wHour, stUTC.wMinute, stUTC.wSecond);
		return string(buffer);
	}
#else
	// sleep
#endif

	string Date::DEFAULT_FORMAT = "%Y-%m-%d %H:%M:%S";

	/**
	 * @brief seconds to string
	 * @ref http://stackoverflow.com/questions/10446526/get-last-modified-time-of-file-in-linux
	 */
	string Date::format(string fmt, TIME seconds) {
		return s_date_format(fmt, seconds);
	}
	

	// file system
#if defined(USE_UNIX_STD)

	static bool s_is_separator(char c) {
		return (c == '/');
	}
	static string s_remove_if_last(string & path, char m) {
		SUPPRESS_WARNING(m);
		if (!path.empty()
			&& path.length() > 1
			&& s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
	static bool s_is_root_path(string & path) {
		return !path.compare("/");
	}
	static bool s_exists(string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/12774207/fastest-way-to-check-if-a-file-exist-using-standard-c-c11-c
		struct stat st;
		return (stat(path.c_str(), &st) == 0);
	}
	static bool s_is_file(string & path) {

		if (path.empty()) {
			return false;
		}
		
		// http://stackoverflow.com/questions/3536781/accessing-directories-in-c/3536853#3536853
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? false : true);
	}
	static bool s_is_directory(string & path) {

		if (path.empty()) {
			return false;
		}
		
		struct stat st;
		lstat(path.c_str(), &st);
		return (S_ISDIR(st.st_mode) ? true : false);
	}
	static bool s_is_writable(string & path) {
		return (access(path.c_str(), W_OK) == 0);
	}
	static string s_get_parent_path(string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

		string p = s_remove_if_last(path, '/');
		size_t f = p.find_last_of("/");
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_if_last(path, '/');
		}

		size_t f = path.find_last_of("/");
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		size_t f = path.find_last_of("/");
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
			string p = s_remove_if_last(path, '/');
			size_t f = p.find_last_of("/");
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(string & path) {
		size_t f = path.find_last_of(".");
		if (f == string::npos) {
			return "";
		}
		return path.substr(f+1);
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

	static TIME s_get_creation_date(string path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return (long int)st.st_ctime;
	}

	static TIME s_get_modified_date(string path) {
		struct stat st;
		if (stat(path.c_str(), &st) != 0) {
			return 0;
		}

		return (long int)st.st_mtime;
	}

	static std::vector<File> s_list(string path) {
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

	static bool s_is_separator(char c);
	static string s_remove_if_last(string & path, char m);
	static bool s_is_fullpath(string & path);
	static bool s_is_root_path(string & path);
	static bool s_exists(string & path);
	static bool s_is_file(string & path);
	static bool s_is_directory(string & path);
	static string s_get_parent_path(string & path);
	static string s_get_path_part(string & path);
	static string s_get_filename_part(string & path);
	static string s_get_entity_name_part(string & path);
	static string s_get_ext(string & path);
	static int s_mkdir(const char *dir, int mode);
	static TIME s_get_creation_date(string path);
	static TIME s_get_modified_date(string path);


	static bool s_is_separator(char c) {
		return (c == '/' || c == '\\');
	}
	static string s_remove_if_last(string & path, char m) {
		if (!path.empty()
			&& path.length() > 1
			&& s_is_separator(*(path.rbegin())) ) {
			return path.substr(0, path.length() - 1); // trailing last / character
		}
		return path;
	}
	static bool s_is_fullpath(string & path) {
		return !path.empty() && s_is_separator(path[0]);
	}
	static bool s_is_root_path(string & path) {
		return !path.compare("/") || !path.compare("\\");
	}
	static bool s_exists(string & path) {

		if (path.empty()) {
			return false;
		}

		if (s_is_directory(path) || s_is_file(path)) {
			return true;
		}
		
		return false;
	}
	static bool s_is_file(string & path) {

		if (path.empty()) {
			return false;
		}

		struct stat s;
		if (stat(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFREG ? true : false);
	}
	static bool s_is_directory(string & path) {

		if (path.empty()) {
			return false;
		}
		
		struct stat s;
		if (stat(path.c_str(), &s) != 0) {
			// error
			return false;
		}

		return (s.st_mode & S_IFDIR ? true : false);
	}
	static bool s_is_writable(string & path) {
		if (!s_exists(path)) {
			return false;
		}
		return (_access(path.c_str(), 2) == 0);
	}
	static string s_get_parent_path(string & path) {

		if (path.empty()) {
			return "";
		}

		if (s_is_root_path(path)) {
			return "";
		}

		string p = s_remove_if_last(path, '\\');
		int f = p.find_last_of("\\");
		if (f == string::npos) {
			return "";
		}
		
		return p.substr(0, f);
	}
	static string s_get_path_part(string & path) {
		
		if (path.empty() || s_is_directory(path) || s_is_root_path(path)) {
			return s_remove_if_last(path, '\\');
		}

		int f = path.find_last_of("\\");
		if (f == string::npos) {
			return path;
		}

		return path.substr(0, f);
	}
	static string s_get_filename_part(string & path) {

		if (path.empty() || s_is_directory(path)) {
			return "";
		}
		
		int f = path.find_last_of("\\");
		if (f == string::npos) {
			return path;
		}

		return path.substr(f + 1);
	}
	static string s_get_entity_name_part(string & path) {
		if (path.empty()) {
			return "";
		}

		if (s_is_directory(path)) {
			string p = s_remove_if_last(path, '\\');
			int f = p.find_last_of("\\");
			if (f == string::npos) {
				return p;
			}
			return p.substr(f+1);
		}

		return s_get_filename_part(path);
	}
	static string s_get_ext(string & path) {
		int f = path.find_last_of(".");
		if (f == string::npos) {
			return "";
		}
		return path.substr(f+1);
	}

	// http://stackoverflow.com/questions/2336242/recursive-mkdir-system-call-on-unix
	static int s_mkdir(const char *dir, int mode) {
	
		// https://msdn.microsoft.com/en-us/library/2fkk4dzw.aspx

		char tmp[256];
		char *p = NULL;
		size_t len;

		snprintf(tmp, sizeof(tmp),"%s",dir);
		len = strlen(tmp);

		if(tmp[len - 1] == '\\') {
			tmp[len - 1] = 0;
		}

		for(p = tmp + 1; *p; p++) {
			if(*p == '\\') {
				*p = 0;
				_mkdir(tmp);
				*p = '\\';
			}
		}
	
		return _mkdir(tmp);


		return 0;
	}
	static TIME s_get_creation_date(string path) {

		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return ftCreate;
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return ftCreate;
		}

		return ftCreate;
	}
	static TIME s_get_modified_date(string path) {
		HANDLE hFile;
		FILETIME ftCreate, ftAccess, ftWrite;
		long int ret = 0;

		memset(&ftCreate, 0, sizeof(ftCreate));
		memset(&ftAccess, 0, sizeof(ftAccess));
		memset(&ftWrite, 0, sizeof(ftWrite));

		hFile = CreateFile(path.c_str(), GENERIC_READ, FILE_SHARE_READ, NULL, OPEN_EXISTING, 0, NULL);
		if (hFile == INVALID_HANDLE_VALUE) {
			return ftWrite;
		}

		if (!GetFileTime(hFile, &ftCreate, &ftAccess, &ftWrite)) {
			return ftWrite;
		}

		return ftWrite;
	}

	static std::vector<File> s_list(string path) {
		std::vector<File> ret;
		return ret;
	}
	
#endif

	/**
	 * @brief file constructor
	 */
	File::File() {
	}
	
	File::File(string path) : path(path) {
	}

	File::~File() {
	}

	bool File::isRootPath(string path){
		return s_is_root_path(path);
	}

	bool File::isFullpath(string path) {
		return s_is_fullpath(path);
	}
	
	bool File::exists(string path){
		return s_exists(path);
	}

	bool File::isFile(string path){
		return s_is_file(path);
	}

	bool File::isDirectory(string path){
		return s_is_directory(path);
	}

	bool File::isWritable(string path) {
		return s_is_writable(path);
	}

	string File::getParentPath(string path) {
		return s_get_parent_path(path);
	}

	string File::getPathPart(string path){
		return s_get_path_part(path);
	}

	string File::getFileNamePart(string path){
		return s_get_filename_part(path);
	}

	string File::getExtension(string path){
		return s_get_ext(path);
	}

	string File::getEntityNamePart(string path) {
		return s_get_entity_name_part(path);
	}

	bool File::compareExtension(string path, string extension){
		
		string a = s_get_ext(path);
		string b = s_get_ext(extension);

		if (a.empty() || b.empty()) {
			return false;
		}

		return (!a.compare(b));
	}

	int File::mkdir(string path) {
		return s_mkdir(path.c_str(), 0755);
	}

	string File::fullpath(string dir, string filename) {
		
		if (dir.length() > 0 && *dir.rbegin() != '/') {
			dir += "/";
		}

		if (filename.length() > 0) {
			size_t f = filename.find_first_not_of('/');
			if (f != string::npos) {
				filename = filename.substr(f);
			} else {
				filename = "";
			}
		}

		return dir + filename;
	}

	string File::getCreationDate(string path, string fmt) {
		TIME t = s_get_creation_date(path);
		return Date::format(fmt, t);
	}
	
	string File::getModifiedDate(string path, string fmt) {
		TIME t = s_get_modified_date(path);
		return Date::format(fmt, t);
	}

	vector<File> File::list(string path) {
		return s_list(path);
	}

	string File::getName() {
		return getEntityNamePart(path);
	}

	string File::toString() {
		return path;
	}
	
} /* OS */
