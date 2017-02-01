#ifndef __SOCKET_API_HPP__
#define __SOCKET_API_HPP__

#include "os.hpp"

namespace OS {

	/**
     * @brief InetAddress
     */
    class InetAddress {
    public:

		/**
		 * @brief InetVersion
		 */
        class InetVersion {
        public:
            static const int UNKNOWN = 0;
            static const int INET4 = 1;
            static const int INET6 = 2;
        private:
            int version;
        public:
            InetVersion() : version(UNKNOWN) {}
            InetVersion(int version) : version(version) {}
            virtual ~InetVersion() {}
            int getVersion();
            bool operator== (int other) const {return version == other;}
            void operator= (int version) {this->version = version;}
        };
        
    private:
        std::string host;
        int port;
        InetVersion inetVersion;
        
    public:
        InetAddress();
        InetAddress(const std::string & host, int port);
		InetAddress(int port);
        InetAddress(struct sockaddr * addr);
        virtual ~InetAddress();
        
		int getFamilyCode() const;
        bool inet4() const;
        bool inet6() const;
        void setInetVersion(int version);
        
        std::string getHost() const;
        void setHost(const std::string & host);
        int getPort() const;
        void setPort(int port);
		void setAddressWithSockAddr(struct sockaddr * addr);
		void setAddress(const InetAddress & addr);
		void setAddress(const std::string & host, int port);
		
		struct addrinfo * resolve(int socktype) const;
		struct addrinfo * resolveNumeric(int socktype) const;
		struct addrinfo * resolvePassive(int family, int socktype) const;

		bool operator==(const InetAddress & other);

		bool valid();

	private:
		static addrinfo * getAddressInfo(const char * node, const char * service, struct addrinfo * hints);
	public:
        
        static std::string getIPAddress(struct sockaddr * addr);
        static int getPort(struct sockaddr * addr);
		static struct addrinfo * getAddrInfo(const char * host, int port, struct addrinfo hints);
    };

	/**
	 * @brief inet 4 address
	 */
	class Inet4Address : public InetAddress {
	private:
	public:
		Inet4Address() { setInetVersion(InetVersion::INET4); }
		Inet4Address(const std::string & host, int port) : InetAddress(host, port) { setInetVersion(InetVersion::INET4); }
		Inet4Address(int port) : InetAddress(port) { setInetVersion(InetVersion::INET4); }
		Inet4Address(struct sockaddr * addr) : InetAddress(addr) { setInetVersion(InetVersion::INET4); }
		virtual ~Inet4Address() {}
	};

	/**
	 * @brief inet 6 address
	 */
	class Inet6Address : public InetAddress {
	private:
	public:
		Inet6Address() { setInetVersion(InetVersion::INET6); }
		Inet6Address(const std::string & host, int port) : InetAddress(host, port) { setInetVersion(InetVersion::INET6); }
		Inet6Address(int port) : InetAddress(port) { setInetVersion(InetVersion::INET6); }
		Inet6Address(struct sockaddr * addr) : InetAddress(addr) { setInetVersion(InetVersion::INET6); }
		virtual ~Inet6Address() {}
	};

	/*
	 * @brief SocketAddress
	 */
	class SocketAddress {
	private:
		struct sockaddr_in in4;
		struct sockaddr_in6 in6;
		struct sockaddr * in;
		socklen_t len;
	public:
		SocketAddress();
		SocketAddress(int spec);
		virtual ~SocketAddress();

		void select(int spec);
		struct sockaddr * getAddr();
		socklen_t * getAddrLen();
	};

	/**
	 * @brief socket util
	 */
	class SocketUtil {
	private:
	public:
		SocketUtil();
		virtual ~SocketUtil();
		static void checkValidSocket(SOCK_HANDLE sock);
		static bool isValidSocket(SOCK_HANDLE sock);
		static void throwSocketException(const std::string & message);
		static void closeSocket(SOCK_HANDLE sock);
        static void setSocketOption(SOCK_HANDLE sock, int level, int optname, const char * optval, int optlen);
	};

	/**
     * @brief GlobalSocketConfiguration
     */
	class GlobalSocketConfiguration {
	private:
		static int preferredInetVersion;
	private:
		GlobalSocketConfiguration();
		virtual ~GlobalSocketConfiguration();

	public:
		static int getPreferredInetVersion();
		static void setPreferredInetVersion(int preferredInetVersion);
	};

	/**
     * @brief SocketOptions
     */
	class SocketOptions {
	private:
		SocketOptions * delegator;
		bool reuseAddr;
		bool broadcast;
		int ttl;
		std::string multicastIface;
	public:
		SocketOptions();
		virtual ~SocketOptions();
		void setDelegator(SocketOptions * delegator);
		virtual void setReuseAddr(bool reuseAddr);
		virtual bool getReuseAddr();
		virtual void setBroadcast(bool broadcast);
		virtual bool getBroadcast();
		virtual void setTimeToLive(int ttl);
		virtual int getTimeToLive();
		virtual void setMulticastInterface(const std::string & iface);
	};

	/**
	 *
	 */
	class SocketAddressResolver {
	private:
		struct addrinfo * info;
	public:
		SocketAddressResolver();
		virtual ~SocketAddressResolver();
		bool resolved();
		void releaseAddrInfo();
		void setAddrInfo(struct addrinfo * info);
		struct addrinfo * getAddrInfo();
	};
	
	/**
	 * @brief Datagram packet
	 */
	class DatagramPacket {
	private:
		char * data;
		size_t size;
		size_t position;
		size_t length;
		InetAddress remoteAddr;
        
	public:
		DatagramPacket(char * data, size_t size);
		DatagramPacket(char * data, size_t size, const InetAddress & remoteAddr);
		DatagramPacket(char * data, size_t size, const std::string & host, int port);
		virtual ~DatagramPacket();
		void clear();
		char * getData();
        const char * getData() const;
        size_t getLength() const;
        size_t getSize() const;
		void write(const unsigned char i8);
		void write(const unsigned short i16);
		void write(const unsigned int i32);
		void write(const unsigned long long i64);
		void write(const char * data, size_t size);
		void write(const std::string & data);
		void setLength(size_t length);
		InetAddress & getRemoteAddr();
		void setRemoteAddr(const InetAddress & addr);
		void setRemoteAddr(const std::string & host, int port);
	};
}

#endif
