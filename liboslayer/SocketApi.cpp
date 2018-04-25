#include "SocketApi.hpp"
#include "Text.hpp"

namespace osl {

	/* InetAddress */

	using namespace std;
    
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
			throw IOException("getaddrinfo() error", -1, 0);
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

	string InetAddress::toString() const {
		return getHost() + ":" + Text::toString(getPort());
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
			throw IOException("Unknown family", -1, 0);
		}
	}
	struct sockaddr * SocketAddress::getAddr() {
		return in;
	}
	socklen_t * SocketAddress::getAddrLen() {
		return &len;
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
		if (isValidSocket(sock) == false) {
			return;
		}
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

	DatagramPacket::DatagramPacket(size_t size)
		: _array(size), position(0), length(0) {
        System::getInstance();
	}

	DatagramPacket::DatagramPacket(size_t size, const InetAddress & remoteAddr)
		: _array(size), position(0), length(0), remoteAddr(remoteAddr) {
		System::getInstance();
	}
	
	DatagramPacket::DatagramPacket(size_t size, const string & host, int port)
		: _array(size), position(0), length(0), remoteAddr(host, port) {
		System::getInstance();
	}

	DatagramPacket::~DatagramPacket() {
	}

	void DatagramPacket::clear() {
		_array.set(0);
		position = 0;
		length = 0;
	}

	char * DatagramPacket::getData() {
		return _array.array();
	}
    
    const char * DatagramPacket::getData() const {
		return _array.const_array();
    }

    size_t DatagramPacket::getLength() const {
        return length;
    }

    size_t DatagramPacket::getSize() const {
		return _array.size();
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
		if (_array.size() < position + size) {
			throw BufferOverflowException("buffer overflowed", -1, 0);
		}
		_array.copy(position, data, size);
		position += size;
		setLength(position);
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
}
