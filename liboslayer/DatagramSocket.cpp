#include "DatagramSocket.hpp"
#include "Utils.hpp"

namespace XOS {
	    
#if defined(USE_BSD_SOCKET)
    
    class DatagramSocketImpl : public DatagramSocket {
    private:
        
        int sock;
        struct addrinfo * res;
        bool reuseAddr;
        
    public:
        DatagramSocketImpl() : sock(-1), res(NULL), reuseAddr(false) {
            sock = ::socket(AF_INET, SOCK_DGRAM, 0);
            if (sock < 0) {
                throw OS::IOException("socket() error", -1, 0);
            }
        }
        DatagramSocketImpl(int port) : sock(-1), res(NULL), reuseAddr(false) {
            OS::InetAddress addr(port);
            bind(addr);
        }
        DatagramSocketImpl(OS::InetAddress & addr) : sock(-1), res(NULL), reuseAddr(false) {
            bind(addr);
        }
        virtual ~DatagramSocketImpl() {
            if (res) {
                freeaddrinfo(res);
            }
        }
        
        void bind(struct addrinfo * addrInfo) {
            
            DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
            AddrInfoAutoRelease infoMem(addrInfo);
            
            sock = ::socket(addrInfo->ai_family, addrInfo->ai_socktype, addrInfo->ai_protocol);
            if (sock < 0) {
                throw OS::IOException("socket() error", -1, 0);
            }
            
            if (reuseAddr) {
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
        
        virtual void bind(OS::InetAddress & addr) {
            bind(addr.resolvePassive(AF_UNSPEC, SOCK_DGRAM));
        }
        virtual void connect(OS::InetAddress & addr) {
            
            res = addr.resolve(SOCK_DGRAM);
            
            sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
            if (sock < 0) {
                throw OS::IOException("socket() error", -1, 0);
            }
            
            if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
                throw OS::IOException("connect() error", -1, 0);
            }
        }
        
        virtual void disconnect() {
            close();
        }

		virtual void close() {
			::close(sock);
			sock = -1;
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
            
            int ret = (int)::recvfrom(sock, packet.getData(), packet.getSize(), 0, client_addr, &client_addr_size);
            
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
            
			DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
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
        
        bool resolvedAddrInfo() {
            return res != NULL;
        }
        
        void setAddrInfo(struct addrinfo * res) {
            if (this->res) {
                freeaddrinfo(this->res);
            }
            this->res = res;
        }
        
        struct addrinfo * getAddrInfo() {
            return res;
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

#elif defined(USE_WINSOCK2)

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
			bind(addr);
		}
		virtual ~DatagramSocketImpl() {
			if (res) {
				freeaddrinfo(res);
			}
		}

		void bind(struct addrinfo * addrInfo) {

			DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
			AddrInfoAutoRelease infoMem(addrInfo);

			sock = ::socket(addrInfo->ai_family, addrInfo->ai_socktype, addrInfo->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

			if (reuseAddr) {
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

		virtual void bind(OS::InetAddress & addr) {
			bind(addr.resolvePassive(AF_UNSPEC, SOCK_DGRAM));
		}
		virtual void connect(OS::InetAddress & addr) {

			setAddrInfo(addr.resolve(SOCK_DGRAM));
			struct addrinfo * res = getAddrInfo();

			sock = ::socket(res->ai_family, res->ai_socktype, res->ai_protocol);
			if (sock == INVALID_SOCKET) {
				throw OS::IOException("socket() error", -1, 0);
			}

			if (::connect(sock, res->ai_addr, res->ai_addrlen) != 0) {
				throw OS::IOException("connect() error", -1, 0);
			}
		}

		virtual void disconnect() {
			close();
		}
		virtual void close() {
			closesocket(sock);
			sock = INVALID_SOCKET;
		}

		virtual int recv(OS::DatagramPacket & packet) {
			
			struct sockaddr_in client_addr4;
			struct sockaddr_in6 client_addr6;

			struct sockaddr * client_addr = NULL;
			socklen_t client_addr_size;

			struct addrinfo * res = getAddrInfo();

			if (res->ai_family == AF_INET) {
				client_addr = (struct sockaddr *)&client_addr4;
				client_addr_size = sizeof(client_addr4);
			} else if (res->ai_family == AF_INET6) {
				client_addr = (struct sockaddr *)&client_addr6;
				client_addr_size = sizeof(client_addr6);
			} else {
				throw OS::IOException("Unknown family", -1, 0);
			}

			int ret = (int)::recvfrom(sock, packet.getData(), packet.getSize(), 0, client_addr, &client_addr_size);

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

			DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
			AddrInfoAutoRelease client_info = packet.getRemoteAddr().resolve(SOCK_DGRAM);

			ret = (int)::sendto(sock, packet.getData(), packet.getLength(), 0, client_info->ai_addr, client_info->ai_addrlen);
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

		bool resolvedAddrInfo() {
			return res != NULL;
		}

		void setAddrInfo(struct addrinfo * res) {
			if (this->res) {
				freeaddrinfo(this->res);
			}
			this->res = res;
		}

		struct addrinfo * getAddrInfo() {
			return res;
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
	void DatagramSocket::close() {
		getImpl().close();
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

	bool DatagramSocket::createdImpl() {
		return impl != NULL;
	}
	void DatagramSocket::setImpl(DatagramSocket * impl) {
		if (this->impl) {
			delete this->impl;
		}
		this->impl = impl;
	}

	void DatagramSocket::createImpl() {
		if (!createdImpl()) {
			setImpl(new DatagramSocketImpl);
		}
	}
	void DatagramSocket::createImpl(int port) {
		if (!createdImpl()) {
			setImpl(new DatagramSocketImpl(port));
		}
	}
	void DatagramSocket::createImpl(OS::InetAddress & addr) {
		if (!createdImpl()) {
			setImpl(new DatagramSocketImpl(addr));
		}
	}
	
	DatagramSocket & DatagramSocket::getImpl() {
		if (!impl) {
			createImpl();
		}
		return *impl;
	}

#if defined(USE_BSD_SOCKET)
    
    class MulticastSocketImpl : public DatagramSocketImpl {
    private:
        OS::InetAddress localAddr;
    public:
        MulticastSocketImpl() {
            setReuseAddr(true);
        }
        MulticastSocketImpl(int port) {
            setReuseAddr(true);
            localAddr.setPort(port);
        }
        MulticastSocketImpl(OS::InetAddress & addr) {
            setReuseAddr(true);
            localAddr.setAddress(addr);
        }
        virtual ~MulticastSocketImpl() {
        }
        
        virtual void joinGroup(const std::string & group) {
            
            OS::InetAddress groupAddr(group, -1);
            DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
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
    
#elif defined(USE_WINSOCK2)

	class MulticastSocketImpl : public DatagramSocketImpl {
	private:
		OS::InetAddress localAddr;
	public:
		MulticastSocketImpl() {
			setReuseAddr(true);
		}
		MulticastSocketImpl(int port) {
			setReuseAddr(true);
			localAddr.setPort(port);
		}
		MulticastSocketImpl(OS::InetAddress & addr) {
			setReuseAddr(true);
			localAddr.setAddress(addr);
		}
		virtual ~MulticastSocketImpl() {
		}

		virtual void joinGroup(const std::string & group) {

			OS::InetAddress groupAddr(group, -1);
			DECL_AUTO_RELEASE(AddrInfoAutoRelease, struct addrinfo, freeaddrinfo);
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

				setOption(IPPROTO_IPV6, IPV6_ADD_MEMBERSHIP, (char*)&mreq, sizeof(mreq));

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
	MulticastSocket::MulticastSocket(OS::InetAddress & addr) {
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
		if (!createdImpl()) {
			setImpl(new MulticastSocketImpl);
		}
	}
	void MulticastSocket::createImpl(int port) {
		if (!createdImpl()) {
			setImpl(new MulticastSocketImpl(port));
		}
	}
	void MulticastSocket::createImpl(OS::InetAddress & addr) {
		if (!createdImpl()) {
			setImpl(new MulticastSocketImpl(addr));
		}
	}
}
