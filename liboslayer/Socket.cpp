#include "Socket.hpp"
#include "AutoRelease.hpp"

namespace osl {

	DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);

#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class SocketImpl : public Socket, public SocketAddressResolver {
	private:
		SOCK_HANDLE sock;
	public:
		SocketImpl() : sock(INVALID_SOCKET) {
		}
		SocketImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : sock(sock) {
			InetAddress inetAddr(addr);
			setAddrInfo(inetAddr.resolve(SOCK_STREAM));
		}
		SocketImpl(const InetAddress & remoteAddr) : sock(INVALID_SOCKET) {
			setRemoteAddress(remoteAddr);
		}
		virtual ~SocketImpl() {
		}
		SOCK_HANDLE getSocket() {
			return sock;
		}
		virtual void setRemoteAddress(const InetAddress & remoteAddr) {
			close();
			setAddrInfo(remoteAddr.resolve(SOCK_STREAM));
			sock = ::socket(getAddrInfo()->ai_family, getAddrInfo()->ai_socktype, getAddrInfo()->ai_protocol);
			SocketUtil::checkValidSocket(sock);
		}
		virtual void connect() {
			if (!resolved()) {
				throw IOException("not resolved", -1, 0);
			}

			if (::connect(sock, getAddrInfo()->ai_addr, getAddrInfo()->ai_addrlen) != 0) {
				SocketUtil::throwSocketException("connect() error");
			}
		}
		virtual void connect(unsigned long timeout) {
			if (!resolved()) {
				throw IOException("not resolved", -1, 0);
			}

			setNonblockingSocket(true);

			int ret = ::connect(sock, getAddrInfo()->ai_addr, getAddrInfo()->ai_addrlen);
			if (ret != 0) {
#if defined(USE_WINSOCK2)
				if (WSAGetLastError() != WSAEWOULDBLOCK) {
#elif defined(USE_BSD_SOCKET)
					if (errno != EINPROGRESS) {
#endif
						SocketUtil::throwSocketException("connect() error");
					}
				}

				Selector selector;
				selector.set(sock, Selector::WRITE);
				if (selector.select(timeout) <= 0) {
					throw IOException("connection timeout", -1, 0);
				}

				int err;
				socklen_t len = sizeof(err);
				getOption(SOL_SOCKET, SO_ERROR, (char *)&err, &len);

				if (err) {
					throw IOException("connect() error", -1, 0);
				}
			
				setNonblockingSocket(false);
			}
			void setNonblockingSocket(bool enable) {
#if defined(USE_WINSOCK2)
				u_long mode = enable ? 1 : 0;
				if (ioctlsocket(sock, FIONBIO, &mode) != 0) {
					SocketUtil::throwSocketException("ioctlsocket() error");
				}
#elif defined(USE_BSD_SOCKET)
				int flags = fcntl(sock, F_GETFL);
				flags = enable ? (flags | O_NONBLOCK) : (flags & ~O_NONBLOCK);
				if (fcntl(sock, F_SETFL, flags) != 0) {
					SocketUtil::throwSocketException("fcntl() error");
				}
#endif
			}
			virtual void disconnect() {
				close();
			}
			virtual void close() {
				SocketUtil::closeSocket(sock);
				sock = INVALID_SOCKET;
				setAddrInfo(NULL);
			}
			virtual int recv(char * buffer, size_t size) {
				int ret = (int)::recv(sock, buffer, size, 0);
				if (ret == 0) {
					throw IOException("normal close", ret, 0);
				} else if (ret < 0) {
					SocketUtil::throwSocketException("recv() error");
				}
				return ret;
			}
			virtual int send(const char * data, size_t size) {
				int ret = (int)::send(sock, data, size, 0);
				if (ret <= 0) {
					SocketUtil::throwSocketException("send() error");
				}
				// TODO: consider - how about requested size and result written size are different
				return ret;
			}
			InetAddress getLocalInetAddress() {
				SocketAddress sa(getAddrInfo()->ai_family);
				if (getsockname(sock, sa.getAddr(), sa.getAddrLen()) != 0) {
					SocketUtil::throwSocketException("getsockname() error");
				}
				return InetAddress(sa.getAddr());
			}
			InetAddress getRemoteInetAddress() {
				if (!resolved()) {
					throw IOException("unresolved socket");
				}
				struct addrinfo * info = getAddrInfo();
				return InetAddress(info->ai_addr);
			}
			virtual void setRecvTimeout(unsigned long timeout) {
				struct timeval tv;
				tv.tv_sec = timeout / 1000;
				tv.tv_usec = (timeout % 1000) * 1000;
				setOption(SOL_SOCKET, SO_RCVTIMEO, (const char*)&tv, sizeof(tv));
			}
			virtual unsigned long getRecvTimeout() {
				struct timeval tv;
				memset(&tv, 0, sizeof(tv));
				socklen_t len = sizeof(tv);
				getOption(SOL_SOCKET, SO_RCVTIMEO, (char *)&tv, &len);
				return (tv.tv_sec * 1000) + (tv.tv_usec / 1000);
			}
			void getOption(int level, int optname, char * optval, socklen_t * optlen) {
				if (getsockopt(sock, level, optname, optval, optlen) != 0) {
					SocketUtil::throwSocketException("getsockopt() error");
				}
			}
			void setOption(int level, int optname, const char * optval, int optlen) {
				if (setsockopt(sock, level, optname, optval, optlen) != 0) {
					SocketUtil::throwSocketException("setsockopt() error");
				}
			}
		};

#endif


		Socket::Socket() : socketImpl(NULL) {
			System::getInstance();
		}
		Socket::Socket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : socketImpl(NULL) {
			System::getInstance();
			createImpl(sock, addr, addrlen);
		}
		Socket::Socket(const InetAddress & remoteAddr) : socketImpl(NULL) {
			System::getInstance();
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
		void Socket::setRemoteAddress(const InetAddress & remoteAddr) {
			getImpl().setRemoteAddress(remoteAddr);
		}
		void Socket::negotiate() {
		}
		void Socket::connect() {
			getImpl().connect();
		}
		void Socket::connect(unsigned long timeout) {
			getImpl().connect(timeout);
		}
		void Socket::disconnect() {
			getImpl().disconnect();
		}
		void Socket::close() {
			getImpl().close();
		}
		bool Socket::isClosed() {
			return SocketUtil::isValidSocket(getSocket()) == false;
		}
		int Socket::pending() {
			return 0;
		}
		int Socket::recv(char * buffer, size_t size) {
			return getImpl().recv(buffer, size);
		}
		int Socket::send(const char * data, size_t size) {
			return getImpl().send(data, size);
		}
		InetAddress Socket::getLocalInetAddress() {
			return getImpl().getLocalInetAddress();
		}
		InetAddress Socket::getRemoteInetAddress() {
			return getImpl().getRemoteInetAddress();
		}
		void Socket::setRecvTimeout(unsigned long timeout) {
			return getImpl().setRecvTimeout(timeout);
		}
		unsigned long Socket::getRecvTimeout() {
			return getImpl().getRecvTimeout();
		}
		void Socket::createImpl() {
			socketImpl = new SocketImpl;
			setDelegator(socketImpl);
		}
		void Socket::createImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) {
			socketImpl = new SocketImpl(sock, addr, addrlen);
			setDelegator(socketImpl);
		}
		void Socket::createImpl(const InetAddress & remoteAddr) {
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

		class ServerSocketImpl : public ServerSocket, public SocketAddressResolver {
		private:
			SOCK_HANDLE sock;
			InetAddress bindAddr;
			SocketOptions options;

		public:
			ServerSocketImpl() : sock(INVALID_SOCKET) {
				bindAddr.setPort(0);
				bindAddr.setInetVersion(GlobalSocketConfiguration::getPreferredInetVersion());
			}
			ServerSocketImpl(int port) : sock(INVALID_SOCKET) {
				bindAddr.setPort(port);
				bindAddr.setInetVersion(GlobalSocketConfiguration::getPreferredInetVersion());
			}
			ServerSocketImpl(const InetAddress & bindAddr) : sock(INVALID_SOCKET) {
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
				SocketUtil::checkValidSocket(sock);

				if (getReuseAddr()) {
					int on = 1;
					setOption(SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on));
#if defined(__APPLE__)
					setOption(SOL_SOCKET, SO_REUSEPORT, (const char*)&on, sizeof(on));
#endif
				}

				if (::bind(sock, addrInfo->ai_addr, addrInfo->ai_addrlen) != 0) {
					throw IOException("bind() error", -1, 0);
				}

				infoMem.forget();
				setAddrInfo(addrInfo);
			}

			void bind(const InetAddress & addr) {
				bind(addr.resolvePassive(addr.getFamilyCode(), SOCK_STREAM));
			}

			virtual void bind() {
				bind(bindAddr);
			}
			virtual void listen(int backlog) {
				if (::listen(sock, backlog) != 0) {
					SocketUtil::throwSocketException("listen() error");
				}
			}
			virtual Socket * accept() {
				SocketAddress sa;
				SOCK_HANDLE client = accept(sa);
				return new Socket(client, sa.getAddr(), *sa.getAddrLen());
			}
			virtual SOCK_HANDLE accept(SocketAddress & addr) {
				if (!resolved()) {
					throw IOException("unresolved socket");
				}

				addr.select(getAddrInfo()->ai_family);

				SOCK_HANDLE client = ::accept(sock, addr.getAddr(), addr.getAddrLen());
				if (client == INVALID_SOCKET) {
					SocketUtil::throwSocketException("accept() error");
				}
			
				return client;
			}
			virtual void close() {
				SocketUtil::closeSocket(sock);
				sock = INVALID_SOCKET;
			}
			InetAddress getLocalInetAddress() {
				SocketAddress sa(getAddrInfo()->ai_family);
				if (getsockname(sock, sa.getAddr(), sa.getAddrLen()) != 0) {
					SocketUtil::throwSocketException("getsockname() error");
				}
				return InetAddress(sa.getAddr());
			}
			void setOption(int level, int optname, const char * optval, int optlen) {
				SocketUtil::setSocketOption(sock, level, optname, optval, optlen);
			}
		};

#endif

		ServerSocket::ServerSocket() : serverSocketImpl(NULL) {
			System::getInstance();
		}
		ServerSocket::ServerSocket(int port) : serverSocketImpl(NULL) {
			System::getInstance();
			createImpl(port);
		}
		ServerSocket::ServerSocket(const InetAddress & bindAddr) : serverSocketImpl(NULL) {
			System::getInstance();
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
		SOCK_HANDLE ServerSocket::accept(SocketAddress & addr) {
			return getImpl().accept(addr);
		}
		void ServerSocket::close() {
			getImpl().close();
		}
		bool ServerSocket::isClosed() {
			return SocketUtil::isValidSocket(getSocket()) == false;
		}
		InetAddress ServerSocket::getLocalInetAddress() {
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
		void ServerSocket::createImpl(const InetAddress & bindAddr) {
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
