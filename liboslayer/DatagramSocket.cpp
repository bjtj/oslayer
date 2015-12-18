#include "DatagramSocket.hpp"
#include "Utils.hpp"

namespace OS {

	DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);

#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class DatagramSocketImpl : public DatagramSocket, public SocketAddressResolver {
	private:
		SOCK_HANDLE sock;
	public:
		DatagramSocketImpl() : sock(INVALID_SOCKET) {
			InetAddress addr(0);
			addr.setInetVersion(GlobalSocketConfiguration::getPreferredInetVersion());
			bind(addr);
		}
		DatagramSocketImpl(int port) : sock(INVALID_SOCKET) {
			InetAddress addr(port);
			addr.setInetVersion(GlobalSocketConfiguration::getPreferredInetVersion());
			bind(addr);
		}
		DatagramSocketImpl(const InetAddress & bindAddr) : sock(INVALID_SOCKET) {
			bind(bindAddr);
		}
        DatagramSocketImpl(DatagramSocketImpl * impl) {
        }
		virtual ~DatagramSocketImpl() {
		}
		SOCK_HANDLE getSocket() {
			return sock;
		}
		void bind(struct addrinfo * addrInfo) {

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
				SocketUtil::throwSocketException("bind() error");
			}

			infoMem.forget();
			setAddrInfo(addrInfo);
		}
		virtual void bind(const InetAddress & bindAddr) {
			bind(bindAddr.resolvePassive(bindAddr.getFamilyCode(), SOCK_DGRAM));
		}
		virtual void connect(const InetAddress & addr) {

			setAddrInfo(addr.resolve(SOCK_DGRAM));
			struct addrinfo * res = getAddrInfo();

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			SocketUtil::checkValidSocket(sock);

			if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw IOException("connect() error", -1, 0);
			}
		}

		virtual void disconnect() {
			close();
		}
		virtual void close() {
			SocketUtil::closeSocket(sock);
			sock = INVALID_SOCKET;
		}

		bool isClosed() {
			return SocketUtil::isValidSocket(sock) == false;
		}

		virtual int recv(DatagramPacket & packet) {

			SocketAddress sa(getAddrInfo()->ai_family);

			int ret = (int)::recvfrom(sock, packet.getData(), packet.getSize(), 0, sa.getAddr(), sa.getAddrLen());
			if (ret <= 0) {
				SocketUtil::throwSocketException("recvfrom() error");
			}
			
			packet.setLength(ret);
			InetAddress remoteAddr(sa.getAddr());
			packet.setRemoteAddr(remoteAddr);
			
			return ret;
		}

		virtual int send(DatagramPacket & packet) {

			AddrInfoAutoRelease remoteInfo(packet.getRemoteAddr().resolve(SOCK_DGRAM));

			int ret = (int)::sendto(sock, packet.getData(), packet.getLength(), 0, remoteInfo->ai_addr, remoteInfo->ai_addrlen);
			if (ret <= 0) {
				SocketUtil::throwSocketException("sendto() error");
			}

			return ret;
		}
		InetAddress getLocalInetAddress() {
			SocketAddress sa(getAddrInfo()->ai_family);
			if (getsockname(sock, sa.getAddr(), sa.getAddrLen()) != 0) {
				SocketUtil::throwSocketException("getsockname() error");
			}
			return InetAddress(sa.getAddr());
		}
		virtual void setMulticastInterface(const std::string & iface) {

			if (!resolved()) {
				throw IOException("Unresolved socket", -1, 0);
			}

			unsigned char optval[sizeof(struct in6_addr)] = {0,};
			int optlen = sizeof(optval);
			int ret = inet_pton(getAddrInfo()->ai_family, iface.c_str(), optval);
			if (ret == 0) {
				throw IOException("inet_pton() error / Invalid Address Format (iface: " + iface + ")", ret, 0);
			} else if (ret < 0) {
				SocketUtil::throwSocketException("inet_pton() error");
			}

			if (getAddrInfo()->ai_family == AF_INET) {
				setOption(IPPROTO_IP, IP_MULTICAST_IF, (const char*)optval, optlen);
			} else if (getAddrInfo()->ai_family == AF_INET6) {
				setOption(IPPROTO_IPV6, IPV6_MULTICAST_IF, (const char*)optval, optlen);
			} else {
				throw IOException("Unknown af family", -1, 0);
			}
		}
		void setOption(int level, int optname, const char * optval, int optlen) {
            SocketUtil::setSocketOption(sock, level, optname, optval, optlen);
		}

		virtual DatagramSocket & getImpl() {
			throw NotImplementedException();
		}
	};

#endif
	
	DatagramSocket::DatagramSocket() : impl(NULL) {
		System::getInstance();
	}
	DatagramSocket::DatagramSocket(int port) : impl(NULL) {
		System::getInstance();
		createImpl(port);
	}
	DatagramSocket::DatagramSocket(const InetAddress & addr) : impl(NULL) {
		System::getInstance();
		createImpl(addr);
	}
	DatagramSocket::~DatagramSocket() {
		if (impl) {
			delete impl;
		}
	}
	SOCK_HANDLE DatagramSocket::getSocket() {
		return getImpl().getSocket();
	}
	int DatagramSocket::getFd() {
		return (int)getSocket();
	}
	void DatagramSocket::bind(const InetAddress & addr) {
		getImpl().bind(addr);
	}
	void DatagramSocket::connect(const InetAddress & addr) {
		getImpl().connect(addr);
	}
	void DatagramSocket::disconnect() {
		getImpl().disconnect();
	}
	void DatagramSocket::close() {
		getImpl().close();
	}
	bool DatagramSocket::isClosed() {
		return getImpl().isClosed();
	}
	int DatagramSocket::recv(DatagramPacket & packet) {
		return getImpl().recv(packet);
	}
	int DatagramSocket::send(DatagramPacket & packet) {
		return getImpl().send(packet);
	}
	InetAddress DatagramSocket::getLocalInetAddress() {
		return getImpl().getLocalInetAddress();
	}
	bool DatagramSocket::created() {
		return impl != NULL;
	}
	void DatagramSocket::setImpl(DatagramSocket * impl) {
		if (this->impl) {
			delete this->impl;
		}
		this->impl = impl;
	}

	void DatagramSocket::createImpl() {
		if (!created()) {
			setImpl(new DatagramSocketImpl);
			setDelegator(impl);
		}
	}
	void DatagramSocket::createImpl(int port) {
		if (!created()) {
			setImpl(new DatagramSocketImpl(port));
			setDelegator(impl);
		}
	}
	void DatagramSocket::createImpl(const InetAddress & addr) {
		if (!created()) {
			setImpl(new DatagramSocketImpl(addr));
			setDelegator(impl);
		}
	}
	
	DatagramSocket & DatagramSocket::getImpl() {
		if (!impl) {
			createImpl();
		}
		return *impl;
	}


#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class MulticastSocketImpl : public DatagramSocketImpl {
	private:
		InetAddress localAddr;
	public:
        MulticastSocketImpl() : DatagramSocketImpl((DatagramSocketImpl*)NULL) {
			setReuseAddr(true);
		}
        MulticastSocketImpl(int port) : DatagramSocketImpl((DatagramSocketImpl*)NULL) {
			setReuseAddr(true);
			localAddr.setPort(port);
		}
		MulticastSocketImpl(const InetAddress & addr) : DatagramSocketImpl((DatagramSocketImpl*)NULL) {
			setReuseAddr(true);
			localAddr.setAddress(addr);
		}
		virtual ~MulticastSocketImpl() {
		}

		virtual void joinGroup(const std::string & group) {

			InetAddress groupAddr(group, -1);
			AddrInfoAutoRelease groupInfo(groupAddr.resolveNumeric(SOCK_DGRAM));

			bind(localAddr.resolvePassive(groupInfo->ai_family, groupInfo->ai_socktype));

			if (groupInfo->ai_family == AF_INET) {
				struct ip_mreq mreq;
				struct sockaddr_in * addr = (struct sockaddr_in*)groupInfo->ai_addr;

				memcpy(&mreq.imr_multiaddr, &(addr->sin_addr), sizeof(mreq.imr_multiaddr));
				mreq.imr_interface.s_addr = htonl(INADDR_ANY);

				setOption(IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof(mreq));
				
			} else if (groupInfo->ai_family == AF_INET6) {

				struct ipv6_mreq mreq;
				struct sockaddr_in6 * addr = (struct sockaddr_in6*)groupInfo->ai_addr;

				memcpy(&(mreq.ipv6mr_multiaddr), &(addr->sin6_addr), sizeof(mreq.ipv6mr_multiaddr));
				mreq.ipv6mr_interface = 0;

				setOption(IPPROTO_IPV6, IPV6_JOIN_GROUP, (char*)&mreq, sizeof(mreq));

			} else {
				throw IOException("Unknown family", -1, 0);
			}
		}
		virtual void setTimeToLive(int ttl) {
			// TODO: implement it
		}
	};

#endif


	MulticastSocket::MulticastSocket() {
	}
	MulticastSocket::MulticastSocket(int port) {
		createImpl(port);
	}
	MulticastSocket::MulticastSocket(const InetAddress & addr) {
		createImpl(addr);
	}
	MulticastSocket::~MulticastSocket() {
	}

	void MulticastSocket::joinGroup(const std::string & group) {
		((MulticastSocketImpl&)getImpl()).joinGroup(group);
	}
	void MulticastSocket::setTimeToLive(int ttl) {
		((MulticastSocketImpl&)getImpl()).setTimeToLive(ttl);
	}
	
	void MulticastSocket::createImpl() {
		if (!created()) {
			new MulticastSocketImpl;
			DatagramSocket * impl = new MulticastSocketImpl;
			setImpl(impl);
			setDelegator(impl);
		}
	}
	void MulticastSocket::createImpl(int port) {
		if (!created()) {
			DatagramSocket * impl = new MulticastSocketImpl(port);
			setImpl(impl);
			setDelegator(impl);
		}
	}
	void MulticastSocket::createImpl(const InetAddress & addr) {
		if (!created()) {
			DatagramSocket * impl = new MulticastSocketImpl(addr);
			setImpl(impl);
			setDelegator(impl);
		}
	}
}
