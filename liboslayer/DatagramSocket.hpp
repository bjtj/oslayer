#ifndef __SOCKET_HPP__
#define __SOCKET_HPP__

#include "os.hpp"

namespace XOS {

	/**
	 * @brief DatagramSocket
	 */
	class DatagramSocket {
	private:
		DatagramSocket * impl;
		int port;

	public:
		DatagramSocket();
		DatagramSocket(int port);
		DatagramSocket(const OS::InetAddress & addr);
		virtual ~DatagramSocket();

		virtual void bind(const OS::InetAddress & addr);
		virtual void connect(const OS::InetAddress & bindAddr);
		virtual void disconnect();
		virtual void close();

		virtual int recv(OS::DatagramPacket & packet);
		virtual int send(OS::DatagramPacket & packet);

		virtual void setReuseAddr(bool reuseAddr);
		virtual bool getReuseAddr();

	protected:
		bool createdImpl();
		void setImpl(DatagramSocket * impl);
		virtual void createImpl();
		virtual void createImpl(int port);
		virtual void createImpl(const OS::InetAddress & bindAddr);
		virtual DatagramSocket & getImpl();
	};


	/**
	 * @brief MulticastSocket
	 */
	class MulticastSocket : public DatagramSocket {
	private:
	public:
		MulticastSocket();
		MulticastSocket(int port);
		MulticastSocket(const OS::InetAddress & addr);
		virtual ~MulticastSocket();

		virtual void joinGroup(const std::string & group);
		virtual void setTimeToLive(int ttl);

	protected:
		virtual void createImpl();
		virtual void createImpl(int port);
		virtual void createImpl(const OS::InetAddress & bindAddr);
	};
}

#endif