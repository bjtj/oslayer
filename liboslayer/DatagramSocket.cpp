#include "DatagramSocket.hpp"
#include "Utils.hpp"

namespace XOS {

	DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);

#if defined(USE_BSD_SOCKET) || defined(USE_WINSOCK2)

	class DatagramSocketImpl : public DatagramSocket, public OS::SocketAddressResolver {
	private:
		SOCK_HANDLE sock;
	public:
		DatagramSocketImpl() : sock(INVALID_SOCKET) {
			OS::InetAddress addr(0);
			addr.setInetVersion(OS::GlobalSocketConfiguration::getPreferedInetVersion());
			bind(addr);
		}
		DatagramSocketImpl(int port) : sock(INVALID_SOCKET) {
			OS::InetAddress addr(port);
			addr.setInetVersion(OS::GlobalSocketConfiguration::getPreferedInetVersion());
			bind(addr);
		}
		DatagramSocketImpl(const OS::InetAddress & addr) : sock(INVALID_SOCKET) {
			bind(addr);
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
			OS::SocketUtil::checkValidSocket(sock);

			if (getReuseAddr()) {
				int on = 1;
				if (::setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on)) != 0) {
					OS::SocketUtil::throwSocketException("setsockopt() error");
				}
			}

			if (::bind(sock, addrInfo->ai_addr, addrInfo->ai_addrlen) != 0) {
				OS::SocketUtil::throwSocketException("bind() error");
			}

			infoMem.forget();
			setAddrInfo(addrInfo);
		}
		virtual void bind(const OS::InetAddress & addr) {
			bind(addr.resolvePassive(addr.getFamilyCode(), SOCK_DGRAM));
		}
		virtual void connect(const OS::InetAddress & addr) {

			setAddrInfo(addr.resolve(SOCK_DGRAM));
			struct addrinfo * res = getAddrInfo();

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			OS::SocketUtil::checkValidSocket(sock);

			if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("connect() error", -1, 0);
			}
		}

		virtual void disconnect() {
			close();
		}
		virtual void close() {
			OS::SocketUtil::closeSocket(sock);
			sock = INVALID_SOCKET;
		}

		virtual int recv(OS::DatagramPacket & packet) {

			OS::SocketAddress sa(getAddrInfo()->ai_family);

			int ret = (int)::recvfrom(sock, packet.getData(), packet.getSize(), 0, sa.getAddr(), sa.getAddrLen());
			if (ret <= 0) {
				OS::SocketUtil::throwSocketException("recvfrom() error");
			}
			
			packet.setLength(ret);
			OS::InetAddress remoteAddr(sa.getAddr());
			packet.setRemoteAddr(remoteAddr);
			
			return ret;
		}

		virtual int send(OS::DatagramPacket & packet) {

			int ret = -1;

			AddrInfoAutoRelease client_info = packet.getRemoteAddr().resolve(SOCK_DGRAM);

			ret = (int)::sendto(sock, packet.getData(), packet.getLength(), 0, client_info->ai_addr, client_info->ai_addrlen);
			if (ret <= 0) {
				OS::SocketUtil::throwSocketException("sendto() error");
			}

			return ret;
		}

		void setOption(int level, int optname, const char * optval, int optlen) {
			if (setsockopt(sock, level, optname, optval, optlen) != 0) {
				throw OS::IOException("setsockopt() error", -1, 0);
			}
		}

		virtual DatagramSocket & getImpl() {
			throw OS::NotImplementedException();
		}
	};

#endif
	
	DatagramSocket::DatagramSocket() : impl(NULL) {
		OS::System::getInstance();
	}
	DatagramSocket::DatagramSocket(int port) : impl(NULL) {
		OS::System::getInstance();
		createImpl(port);
	}
	DatagramSocket::DatagramSocket(const OS::InetAddress & addr) : impl(NULL) {
		OS::System::getInstance();
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
	void DatagramSocket::bind(const OS::InetAddress & addr) {
		getImpl().bind(addr);
	}
	void DatagramSocket::connect(const OS::InetAddress & addr) {
		getImpl().connect(addr);
	}
	void DatagramSocket::disconnect() {
		getImpl().disconnect();
	}
	void DatagramSocket::close() {
		getImpl().close();
	}
	int DatagramSocket::recv(OS::DatagramPacket & packet) {
		return getImpl().recv(packet);
	}
	int DatagramSocket::send(OS::DatagramPacket & packet) {
		return getImpl().send(packet);
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
	void DatagramSocket::createImpl(const OS::InetAddress & addr) {
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
		OS::InetAddress localAddr;
	public:
        MulticastSocketImpl() : DatagramSocketImpl(this) {
			setReuseAddr(true);
		}
        MulticastSocketImpl(int port) : DatagramSocketImpl(this) {
			setReuseAddr(true);
			localAddr.setPort(port);
		}
		MulticastSocketImpl(const OS::InetAddress & addr) : DatagramSocketImpl(this) {
			setReuseAddr(true);
			localAddr.setAddress(addr);
		}
		virtual ~MulticastSocketImpl() {
		}

		virtual void joinGroup(const std::string & group) {

			OS::InetAddress groupAddr(group, -1);
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
				throw OS::IOException("Unknown family", -1, 0);
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
	MulticastSocket::MulticastSocket(const OS::InetAddress & addr) {
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
	void MulticastSocket::createImpl(const OS::InetAddress & addr) {
		if (!created()) {
			DatagramSocket * impl = new MulticastSocketImpl(addr);
			setImpl(impl);
			setDelegator(impl);
		}
	}
}
