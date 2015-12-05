#include "Socket.hpp"
#include "AutoRelease.hpp"

namespace XOS {

	static int PREFERED_INET_VERSION = OS::InetAddress::InetVersion::INET4;
	DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);

#if defined(USE_WINSOCK2)

	class SocketImpl : public Socket {
	private:
		SOCK_HANDLE sock;
		struct addrinfo * info;
	public:
		SocketImpl() : sock(INVALID_SOCKET), info(NULL) {
		}
		SocketImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : sock(sock), info(NULL) {
			OS::InetAddress inetAddr(addr);
			info = inetAddr.resolve(SOCK_STREAM);
		}
		SocketImpl(const OS::InetAddress & remoteAddr) : sock(INVALID_SOCKET), info(NULL) {
			connect(remoteAddr);
		}
		virtual ~SocketImpl() {
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
			closesocket(sock);
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
		bool resolvedAddrInfo() {
			return info != NULL;
		}
		void setAddrInfo(struct addrinfo * info) {
			if (this->info) {
				freeaddrinfo(this->info);
			}
			this->info = info;
		}
		struct addrinfo * getAddrInfo() {
			return info;
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

	void Socket::connect(const OS::InetAddress & remoteAddr) {
		getImpl().connect(remoteAddr);
	}
	void Socket::disconnect() {
		getImpl().disconnect();
	}
	void Socket::close() {
		getImpl().close();
	}

	int Socket::recv(char * buffer, size_t size) {
		return getImpl().recv(buffer, size);
	}
	int Socket::send(const char * data, size_t size) {
		return getImpl().send(data, size);
	}
	void Socket::createImpl() {
		socketImpl = new SocketImpl;
	}
	void Socket::createImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) {
		socketImpl = new SocketImpl(sock, addr, addrlen);
	}
	void Socket::createImpl(const OS::InetAddress & remoteAddr) {
		socketImpl = new SocketImpl(remoteAddr);
	}
	Socket & Socket::getImpl() {
		if (!socketImpl) {
			createImpl();
		}
		return *socketImpl;
	}


#if defined(USE_WINSOCK2)

	class ServerSocketImpl : public ServerSocket {
	private:
		SOCK_HANDLE sock;
		struct addrinfo * info;
		OS::InetAddress bindAddr;
	public:
		ServerSocketImpl() : sock(INVALID_SOCKET), info(NULL) {
			bindAddr.setPort(0);
			bindAddr.setInetVersion(PREFERED_INET_VERSION);
		}
		ServerSocketImpl(int port) : sock(INVALID_SOCKET), info(NULL) {
			bindAddr.setPort(port);
			bindAddr.setInetVersion(PREFERED_INET_VERSION);
		}
		ServerSocketImpl(const OS::InetAddress & bindAddr) : sock(INVALID_SOCKET), info(NULL) {
			this->bindAddr.setAddress(bindAddr);
		}
		virtual ~ServerSocketImpl() {
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

			if (!resolvedAddrInfo()) {
				throw OS::IOException("unresolved socket", -1, 0);
			}

			struct sockaddr_in addr4;
			struct sockaddr_in6 addr6;
			struct sockaddr * addr;
			socklen_t addrlen;

			if (getAddrInfo()->ai_family == AF_INET) {
				addr = (struct sockaddr *)&addr4;
				addrlen = sizeof(addr4);
			} else if (getAddrInfo()->ai_family == AF_INET6) {
				addr = (struct sockaddr *)&addr6;
				addrlen = sizeof(addr6);
			} else {
				throw OS::IOException("Unknown family", -1, 0);
			}

			SOCK_HANDLE client = ::accept(sock, addr, &addrlen);
			if (client == INVALID_SOCKET) {
				OS::SocketUtil::throwSocketException("accept() error");
			}

			return new Socket(client, addr, addrlen);
		}
		virtual void close() {
			closesocket(sock);
			sock = INVALID_SOCKET;
		}

		bool resolvedAddrInfo() {
			return info != NULL;
		}
		void setAddrInfo(struct addrinfo * info) {
			if (this->info) {
				freeaddrinfo(this->info);
			}
			this->info = info;
		}
		struct addrinfo * getAddrInfo() {
			return info;
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

	void ServerSocket::createImpl() {
		serverSocketImpl = new ServerSocketImpl;
	}
	void ServerSocket::createImpl(int port) {
		serverSocketImpl = new ServerSocketImpl(port);
	}
	void ServerSocket::createImpl(const OS::InetAddress & bindAddr) {
		serverSocketImpl = new ServerSocketImpl(bindAddr);
	}
	ServerSocket & ServerSocket::getImpl() {
		if (!serverSocketImpl) {
			createImpl();
		}
		return *serverSocketImpl;
	}
}