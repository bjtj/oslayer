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
		OS::InetAddress localAddr;
		OS::InetAddress remoteAddr;
		int port;

	public:
		DatagramSocket();
		virtual ~DatagramSocket();
		/*DatagramSocket(int port);
		DatagramSocket(int port, OS::InetAddress addr);*/

		virtual void bind(int port);
		virtual void connect(OS::InetAddress & addr);
		virtual void disconnect();

		virtual int recv(OS::DatagramPacket & packet);
		virtual int send(OS::DatagramPacket & packet);

		OS::InetAddress & getLocalAddress();
		OS::InetAddress & getRemoteAddress();

		void setReuseAddr(bool reuseAddr);
		bool getReuseAddr();

	protected:
		bool hasImpl();
		virtual void setImpl(DatagramSocket * impl);
		virtual DatagramSocket & getImpl();
	};



	class MulticastSocket : public DatagramSocket {
	private:
	public:
		MulticastSocket();
		virtual ~MulticastSocket();
		//DatagramSocket(int port);
		//DatagramSocket(int port, OS::InetAddress addr);

		//virtual void bind(int port);
		//virtual void connect(OS::InetAddress & addr);
		//virtual void disconnect();
		virtual void joinGroup(OS::InetAddress & addr);
		virtual void setTimeToLive(int ttl);

		//virtual int recv(OS::DatagramPacket & packet);
		//virtual int send(OS::DatagramPacket & packet);

		//OS::InetAddress & getLocalAddress();
		//OS::InetAddress & getRemoteAddress();

	protected:
		virtual DatagramSocket & getImpl();
		virtual MulticastSocket & getMulticastImpl();
	};

}

#endif