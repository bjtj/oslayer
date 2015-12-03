#ifndef __SOCKET_HPP__
#define __SOCKET_HPP__

#include "os.hpp"

namespace XOS {

	class SocketUtil {
	private:
	public:
		static struct addrinfo * getAddrInfo(const char * host, int port, struct addrinfo hints);
	};

	class DatagramSocket {
	private:
		DatagramSocket * impl;
		int port;

	public:
		DatagramSocket();
		DatagramSocket(int port);
		DatagramSocket(OS::InetAddress & addr);
		virtual ~DatagramSocket();

		virtual void bind(OS::InetAddress & addr);
		virtual void connect(OS::InetAddress & addr);
		virtual void disconnect();

		virtual int recv(OS::DatagramPacket & packet);
		virtual int send(OS::DatagramPacket & packet);

		virtual void setReuseAddr(bool reuseAddr);
		virtual bool getReuseAddr();

	protected:
		void createImpl();
		void createImpl(int port);
		void createImpl(OS::InetAddress & addr);
		virtual DatagramSocket & getImpl();
	};
}

#endif