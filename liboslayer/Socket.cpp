#include "Socket.hpp"
#include "AutoRelease.hpp"

namespace XOS {

	class AddrInfoAutoRelease : public UTIL::AutoRelease<struct addrinfo> {
	private:
	public:
		AddrInfoAutoRelease(struct addrinfo * target) : UTIL::AutoRelease<struct addrinfo>(target) {}
		virtual ~AddrInfoAutoRelease() {}
		virtual void release(struct addrinfo * target) {
			if (target) {
				freeaddrinfo(target);
			}
		}
	};

	struct addrinfo * SocketUtil::getAddrInfo(const char * host, int port, struct addrinfo hints) {
		struct addrinfo * res;
		char portStr[10] = {0,};
		snprintf(portStr, sizeof(portStr), "%d", port);
		memset(&hints, 0, sizeof(hints));
		if (getaddrinfo(host, (port == 0 ? NULL : portStr), &hints, &res) != 0) {
			throw OS::IOException("getaddrinfo() error", -1, 0);
		}
		return res;
	}

#if defined(USE_WINSOCK2)

	class DatagramSocketImpl : public DatagramSocket {
	private:

		SOCKET sock;

	public:
		DatagramSocketImpl(int port) : sock(INVALID_SOCKET) {

			struct addrinfo hints;
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			hints.ai_flags = AI_PASSIVE;

			AddrInfoAutoRelease res(SocketUtil::getAddrInfo(NULL, port, hints));

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

		}
		virtual ~DatagramSocketImpl() {
		}

		virtual void bind(int port) {

			// TODO: implement it
			/*if (::bind(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("bind() error", -1, 0);
			}

			getLocalAddress().setAddress(res->ai_addr);
			getLocalAddress().setPort(port);*/
		}
		virtual void connect(OS::InetAddress & addr) {

			struct addrinfo hints;
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_socktype = SOCK_DGRAM;
			hints.ai_protocol = IPPROTO_UDP;

			AddrInfoAutoRelease res(SocketUtil::getAddrInfo(addr.getHost().c_str(), addr.getPort(), hints));

			if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("connect() error", -1, 0);
			}

			getRemoteAddress().setAddress(addr);
		}

		virtual void disconnect() {
			closesocket(sock);
			sock = INVALID_SOCKET;
		}

		virtual int recv(OS::DatagramPacket & packet) {
		}

		virtual int send(OS::DatagramPacket & packet) {
		}

		virtual DatagramSocket & getImpl() {
			throw OS::NotImplementedException();
		}
	};

#endif
	
	DatagramSocket::DatagramSocket() : impl(NULL) {
		OS::System::getInstance();
	}
	DatagramSocket::~DatagramSocket() {
	}
	/*DatagramSocket(int port);
	DatagramSocket(int port, OS::InetAddress addr);*/

	void DatagramSocket::bind(int port) {
		getImpl().bind(port);
	}
	void DatagramSocket::connect(OS::InetAddress & addr) {
		getImpl().connect(addr);
	}
	void DatagramSocket::disconnect() {
		getImpl().disconnect();
	}
	int DatagramSocket::recv(OS::DatagramPacket & packet) {
		return getImpl().recv(packet);
	}
	int DatagramSocket::send(OS::DatagramPacket & packet) {
		return getImpl().send(packet);
	}
	OS::InetAddress & DatagramSocket::getLocalAddress() {
		return localAddr;
	}
	OS::InetAddress & DatagramSocket::getRemoteAddress() {
		return remoteAddr;
	}

	void DatagramSocket::setReuseAddr(bool reuseAddr) {
		
	}
	bool DatagramSocket::getReuseAddr() {
		return false;
	}

	bool DatagramSocket::hasImpl() {
		return impl != NULL;
	}

	void DatagramSocket::setImpl(DatagramSocket * impl) {
		this->impl = impl;
	}

	DatagramSocket & DatagramSocket::getImpl() {
		if (!impl) {
			// TODO: implement it
			// impl = new DatagramSocketImpl;
		}
		return *impl;
	}


#if defined(USE_WINSOCK2)

	class MulticastSocketImpl : public MulticastSocket {
	private:
		SOCKET sock;
	public:
		MulticastSocketImpl() : sock(INVALID_SOCKET) {
		}

		virtual ~MulticastSocketImpl() {
		}

		virtual void joinGroup(OS::InetAddress & addr) {

			struct addrinfo hints;
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = AF_UNSPEC;
			hints.ai_flags = AI_NUMERICHOST;

			AddrInfoAutoRelease groupRes(SocketUtil::getAddrInfo(addr.getHost().c_str(), 0, hints));
			
			memset(&hints, 0, sizeof(hints));
			hints.ai_family = groupRes->ai_family;
			hints.ai_protocol = SOCK_DGRAM;
			hints.ai_flags = AI_PASSIVE;

			AddrInfoAutoRelease localRes(SocketUtil::getAddrInfo(NULL, addr.getPort(), hints));

			sock = ::socket(localRes->ai_family, localRes->ai_socktype, localRes->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

			if (::bind(sock, localRes->ai_addr, localRes->ai_addrlen) != 0) {
				throw OS::IOException("bind() error", -1, 0);
			}

			if (groupRes->ai_family == AF_INET) {

				struct ip_mreq mreq;
				struct sockaddr_in * addr = (struct sockaddr_in*)groupRes->ai_addr;
				memcpy(&mreq.imr_multiaddr, &(addr->sin_addr), sizeof(mreq.imr_multiaddr));
				mreq.imr_interface.s_addr = htonl(INADDR_ANY);
				if (::setsockopt(sock, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char*)&mreq, sizeof(mreq)) != 0) {
					throw OS::IOException("setsockopt() error", -1, 0);
				}

			} else if (groupRes->ai_family == AF_INET6) {

				struct ipv6_mreq mreq;
				struct sockaddr_in6 * addr = (struct sockaddr_in6*)groupRes->ai_addr;

				memcpy(&(mreq.ipv6mr_multiaddr), &(addr->sin6_addr), sizeof(mreq.ipv6mr_multiaddr));
				mreq.ipv6mr_interface = 0;

				if (setsockopt(sock, IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, (char*)&mreq, sizeof(mreq)) != 0) {
					throw OS::IOException("setsockopt() error", -1, 0);
					return;
				}

			} else {
				throw OS::IOException("Unknown family", -1, 0);
			}
		}

		virtual void setTimeToLive(int ttl) {
		}


		DatagramSocket & getImpl() {
			throw OS::NotImplementedException();
		}
	};

#endif


	MulticastSocket::MulticastSocket() {
		OS::System::getInstance();
	}
	MulticastSocket::~MulticastSocket() {
	}
	//DatagramSocket(int port);
	//DatagramSocket(int port, OS::InetAddress addr);

	//virtual void bind(int port);
	//virtual void connect(OS::InetAddress & addr);
	//virtual void disconnect();
	void MulticastSocket::joinGroup(OS::InetAddress & addr) {
		getMulticastImpl().joinGroup(addr);
	}
	void MulticastSocket::setTimeToLive(int ttl) {
		getMulticastImpl().setTimeToLive(ttl);
	}

	//virtual int recv(OS::DatagramPacket & packet);
	//virtual int send(OS::DatagramPacket & packet);

	//OS::InetAddress & getLocalAddress();
	//OS::InetAddress & getRemoteAddress();


	DatagramSocket & MulticastSocket::getImpl() {
		if (!hasImpl()) {
			setImpl(new MulticastSocketImpl);
		}
		return DatagramSocket::getImpl();
	}
	MulticastSocket & MulticastSocket::getMulticastImpl() {
		return (MulticastSocket&)getImpl();
	}
}
