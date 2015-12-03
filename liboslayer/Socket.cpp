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
		struct addrinfo * res;
		bool reuseAddr;

	public:
		DatagramSocketImpl() : sock(INVALID_SOCKET), res(NULL), reuseAddr(false) {
			sock = ::socket(AF_INET, SOCK_DGRAM, 0);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}
		}
		DatagramSocketImpl(int port) : sock(INVALID_SOCKET), res(NULL), reuseAddr(false) {
			OS::InetAddress addr(port);
			bind(addr);
		}
		DatagramSocketImpl(OS::InetAddress & addr) : sock(INVALID_SOCKET), res(NULL), reuseAddr(false) {
			connect(addr);
		}
		virtual ~DatagramSocketImpl() {
			if (res) {
				freeaddrinfo(res);
			}
		}

		virtual void bind(OS::InetAddress & addr) {

			res = addr.resolvePassive(AF_INET, SOCK_DGRAM);

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

			if (reuseAddr) {
				int on = 1;
				if (::setsockopt(sock, SOL_SOCKET, SO_REUSEADDR, (const char*)&on, sizeof(on)) != 0) {
					throw OS::IOException("setsockopt() error", -1, 0);
				}
			}

			if (::bind(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("bind() error", -1, 0);
			}
		}
		virtual void connect(OS::InetAddress & addr) {

			res = addr.resolve(SOCK_DGRAM);

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

			if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("connect() error", -1, 0);
			}
		}

		virtual void disconnect() {
			closesocket(sock);
			sock = INVALID_SOCKET;
		}

		virtual int recv(OS::DatagramPacket & packet) {
			
			struct sockaddr_in client_addr4;
			struct sockaddr_in6 client_addr6;

			struct sockaddr * client_addr = NULL;
			socklen_t client_addr_size;

			if (res->ai_family == AF_INET) {
				client_addr = (struct sockaddr *)&client_addr4;
				client_addr_size = sizeof(client_addr4);
			} else if (res->ai_family == AF_INET6) {
				client_addr = (struct sockaddr *)&client_addr6;
				client_addr_size = sizeof(client_addr6);
			} else {
				throw OS::IOException("Unknown family", -1, 0);
			}

			int ret = (int)::recvfrom(sock, packet.getData(), packet.getMaxSize(), 0, client_addr, &client_addr_size);

			if (ret <= 0) {
				throw OS::IOException("recvfrom() error", -1, 0);
			}
			
			packet.setLength(ret);
			OS::InetAddress remoteAddr(client_addr);
			packet.setRemoteAddr(remoteAddr);
			
			return ret;
		}

		virtual int send(OS::DatagramPacket & packet) {

			int ret = -1;
			char portstr[10] = {0,};

			AddrInfoAutoRelease autoRel(packet.getRemoteAddr().resolve(SOCK_DGRAM));
			
			ret = (int)::sendto(sock, packet.getData(), packet.getLength(), 0, autoRel->ai_addr, autoRel->ai_addrlen);
			if (ret <= 0) {
				throw OS::IOException("sendto() error", -1, 0);
			}

			return ret;
		}

		virtual void setReuseAddr(bool reuseAddr) {
			this->reuseAddr = reuseAddr;
		}
		virtual bool getReuseAddr() {
			return reuseAddr;
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
	DatagramSocket::DatagramSocket(OS::InetAddress & addr) : impl(NULL) {
		OS::System::getInstance();
		createImpl(addr);
	}
	DatagramSocket::~DatagramSocket() {
		if (impl) {
			delete impl;
		}
	}

	void DatagramSocket::bind(OS::InetAddress & addr) {
		getImpl().bind(addr);
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

	void DatagramSocket::setReuseAddr(bool reuseAddr) {
		getImpl().setReuseAddr(reuseAddr);
	}
	bool DatagramSocket::getReuseAddr() {
		return getImpl().getReuseAddr();
	}

	void DatagramSocket::createImpl() {
		if (!impl) {
			impl = new DatagramSocketImpl;
		}
	}
	void DatagramSocket::createImpl(int port) {
		if (!impl) {
			impl = new DatagramSocketImpl(port);
		}
	}
	void DatagramSocket::createImpl(OS::InetAddress & addr) {
		if (!impl) {
			impl = new DatagramSocketImpl(addr);
		}
	}
	
	DatagramSocket & DatagramSocket::getImpl() {
		if (!impl) {
			createImpl();
		}
		return *impl;
	}
}
