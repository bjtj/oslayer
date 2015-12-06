#include "Socket.hpp"
#include "AutoRelease.hpp"

namespace XOS {

	DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);

#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class SocketImpl : public Socket, public OS::SocketAddressResolver {
	private:
		SOCK_HANDLE sock;
	public:
		SocketImpl() : sock(INVALID_SOCKET) {
		}
		SocketImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : sock(sock) {
			OS::InetAddress inetAddr(addr);
			setAddrInfo(inetAddr.resolve(SOCK_STREAM));
		}
		SocketImpl(const OS::InetAddress & remoteAddr) : sock(INVALID_SOCKET) {
			connect(remoteAddr);
		}
		virtual ~SocketImpl() {
		}
		SOCK_HANDLE getSocket() {
			return sock;
		}
		virtual void connect(const OS::InetAddress & remoteAddr) {
			AddrInfoAutoRelease info(remoteAddr.resolve(SOCK_STREAM));

			sock = ::socket(info->ai_family, info->ai_socktype, info->ai_protocol);
			OS::SocketUtil::checkValidSocket(sock);

			if (::connect(sock, info->ai_addr, info->ai_addrlen) != 0) {
				OS::SocketUtil::throwSocketException("connect() error");
			}

			info.forget();
			setAddrInfo(&info);
		}
		virtual void disconnect() {
			close();
		}
		virtual void close() {
			OS::SocketUtil::closeSocket(sock);
			sock = INVALID_SOCKET;
		}
		virtual int recv(char * buffer, size_t size) {
			int ret = (int)::recv(sock, buffer, size, 0);
			if (ret <= 0) {
				OS::SocketUtil::throwSocketException("recv() error");
			}
			return ret;
		}
		virtual int send(const char * data, size_t size) {
			int ret = (int)::send(sock, data, size, 0);
			if (ret <= 0) {
				OS::SocketUtil::throwSocketException("recv() error");
			}
			return ret;
		}
		OS::InetAddress getLocalInetAddress() {
			OS::SocketAddress sa(getAddrInfo()->ai_family);
			if (getsockname(sock, sa.getAddr(), sa.getAddrLen()) != 0) {
				OS::SocketUtil::throwSocketException("getsockname() error");
			}
			return OS::InetAddress(sa.getAddr());
		}
		OS::InetAddress getRemoteInetAddress() {
			if (!resolved()) {
				throw OS::IOException("unresolved socket", -1, 0);
			}
			struct addrinfo * info = getAddrInfo();
			return OS::InetAddress(info->ai_addr);
		}
		void setOption(int level, int optname, const char * optval, int optlen) {
			if (setsockopt(sock, level, optname, optval, optlen) != 0) {
				OS::SocketUtil::throwSocketException("setsockopt() error");
			}
		}
	};

#endif


	Socket::Socket() : socketImpl(NULL) {
		OS::System::getInstance();
	}
	Socket::Socket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : socketImpl(NULL) {
		OS::System::getInstance();
		createImpl(sock, addr, addrlen);
	}
	Socket::Socket(const OS::InetAddress & remoteAddr) : socketImpl(NULL) {
		OS::System::getInstance();
		createImpl(remoteAddr);
	}
	Socket::~Socket() {
		if (socketImpl) {
			delete socketImpl;
		}
	}
	SOCK_HANDLE Socket::getSocket() {
		return getImpl().getSocket();
	}
	int Socket::getFd() {
		return (int)getSocket();
	}
	void Socket::connect(const OS::InetAddress & remoteAddr) {
		getImpl().connect(remoteAddr);
	}
	void Socket::disconnect() {
		getImpl().disconnect();
	}
	void Socket::close() {
		getImpl().close();
	}
	bool Socket::isClosed() {
		return OS::SocketUtil::isValidSocket(getSocket()) == false;
	}
	int Socket::recv(char * buffer, size_t size) {
		return getImpl().recv(buffer, size);
	}
	int Socket::send(const char * data, size_t size) {
		return getImpl().send(data, size);
	}
	OS::InetAddress Socket::getLocalInetAddress() {
		return getImpl().getLocalInetAddress();
	}
	OS::InetAddress Socket::getRemoteInetAddress() {
		return getImpl().getRemoteInetAddress();
	}
	void Socket::createImpl() {
		socketImpl = new SocketImpl;
		setDelegator(socketImpl);
	}
	void Socket::createImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) {
		socketImpl = new SocketImpl(sock, addr, addrlen);
		setDelegator(socketImpl);
	}
	void Socket::createImpl(const OS::InetAddress & remoteAddr) {
		socketImpl = new SocketImpl(remoteAddr);
		setDelegator(socketImpl);
	}
	Socket & Socket::getImpl() {
		if (!socketImpl) {
			createImpl();
		}
		return *socketImpl;
	}


#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class ServerSocketImpl : public ServerSocket, public OS::SocketAddressResolver {
	private:
		SOCK_HANDLE sock;
		OS::InetAddress bindAddr;
		OS::SocketOptions options;

	public:
		ServerSocketImpl() : sock(INVALID_SOCKET) {
			bindAddr.setPort(0);
			bindAddr.setInetVersion(OS::GlobalSocketConfiguration::getPreferedInetVersion());
		}
		ServerSocketImpl(int port) : sock(INVALID_SOCKET) {
			bindAddr.setPort(port);
			bindAddr.setInetVersion(OS::GlobalSocketConfiguration::getPreferedInetVersion());
		}
		ServerSocketImpl(const OS::InetAddress & bindAddr) : sock(INVALID_SOCKET) {
			this->bindAddr.setAddress(bindAddr);
		}
		virtual ~ServerSocketImpl() {
		}
		SOCK_HANDLE getSocket() {
			return sock;
		}
		void bind(struct addrinfo * addrInfo) {

			DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
			AddrInfoAutoRelease infoMem(addrInfo);

			sock = ::socket(addrInfo->ai_family, addrInfo->ai_socktype, addrInfo->ai_protocol);
			OS::SocketUtil::checkValidSocket(sock);

			if (getReuseAddr()) {
				int on = 1;
				if (::setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on)) != 0) {
					throw OS::IOException("setsockopt() error", -1, 0);
				}
			}

			if (::bind(sock, addrInfo->ai_addr, addrInfo->ai_addrlen) != 0) {
				throw OS::IOException("bind() error", -1, 0);
			}

			infoMem.forget();
			setAddrInfo(addrInfo);
		}

		void bind(const OS::InetAddress & addr) {
			bind(addr.resolvePassive(addr.getFamilyCode(), SOCK_STREAM));
		}

		virtual void bind() {
			bind(bindAddr);
		}
		virtual void listen(int queueLimit) {
			if (::listen(sock, queueLimit) != 0) {
				OS::SocketUtil::throwSocketException("listen() error");
			}
		}
		virtual Socket * accept() {

			if (!resolved()) {
				throw OS::IOException("unresolved socket", -1, 0);
			}

			OS::SocketAddress sa(getAddrInfo()->ai_family);

			SOCK_HANDLE client = ::accept(sock, sa.getAddr(), sa.getAddrLen());
			if (client == INVALID_SOCKET) {
				OS::SocketUtil::throwSocketException("accept() error");
			}

			return new Socket(client, sa.getAddr(), *sa.getAddrLen());
		}
		virtual void close() {
			OS::SocketUtil::closeSocket(sock);
			sock = INVALID_SOCKET;
		}
		OS::InetAddress getLocalInetAddress() {
			OS::SocketAddress sa(getAddrInfo()->ai_family);
			if (getsockname(sock, sa.getAddr(), sa.getAddrLen()) != 0) {
				OS::SocketUtil::throwSocketException("getsockname() error");
			}
			return OS::InetAddress(sa.getAddr());
		}
		void setOption(int level, int optname, const char * optval, int optlen) {
			if (setsockopt(sock, level, optname, optval, optlen) != 0) {
				OS::SocketUtil::throwSocketException("setsockopt() error");
			}
		}
	};

#endif


	ServerSocket::ServerSocket() : serverSocketImpl(NULL) {
		OS::System::getInstance();
	}
	ServerSocket::ServerSocket(int port) : serverSocketImpl(NULL) {
		OS::System::getInstance();
		createImpl(port);
	}
	ServerSocket::ServerSocket(const OS::InetAddress & bindAddr) : serverSocketImpl(NULL) {
		OS::System::getInstance();
		createImpl(bindAddr);
	}
	ServerSocket::~ServerSocket() {
		if (serverSocketImpl) {
			delete serverSocketImpl;
		}
	}
	SOCK_HANDLE ServerSocket::getSocket() {
		return getImpl().getSocket();
	}
	int ServerSocket::getFd() {
		return (int)getSocket();
	}
	void ServerSocket::bind() {
		getImpl().bind();
	}
	void ServerSocket::listen(int queueLimit) {
		getImpl().listen(queueLimit);
	}
	Socket * ServerSocket::accept() {
		return getImpl().accept();
	}
	void ServerSocket::close() {
		getImpl().close();
	}
	bool ServerSocket::isClosed() {
		return OS::SocketUtil::isValidSocket(getSocket()) == false;
	}
	OS::InetAddress ServerSocket::getLocalInetAddress() {
		return getImpl().getLocalInetAddress();
	}
	void ServerSocket::createImpl() {
		serverSocketImpl = new ServerSocketImpl;
		setDelegator(serverSocketImpl);
	}
	void ServerSocket::createImpl(int port) {
		serverSocketImpl = new ServerSocketImpl(port);
		setDelegator(serverSocketImpl);
	}
	void ServerSocket::createImpl(const OS::InetAddress & bindAddr) {
		serverSocketImpl = new ServerSocketImpl(bindAddr);
		setDelegator(serverSocketImpl);
	}
	ServerSocket & ServerSocket::getImpl() {
		if (!serverSocketImpl) {
			createImpl();
		}
		return *serverSocketImpl;
	}
}