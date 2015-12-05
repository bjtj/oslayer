#ifndef __SOCKET_HPP__
#define __SOCKET_HPP__

#include "os.hpp"

namespace XOS {

	/**
	 * @brief Socket
	 */
	class Socket {
	private:
		Socket * socketImpl;
	public:
		Socket();
		Socket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		Socket(const OS::InetAddress & remoteAddr);
		virtual ~Socket();

		virtual void connect(const OS::InetAddress & remoteAddr);
		virtual void disconnect();
		virtual void close();

		virtual int recv(char * buffer, size_t size);
		virtual int send(const char * data, size_t size);

	protected:
		virtual void createImpl();
		virtual void createImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		virtual void createImpl(const OS::InetAddress & remoteAddr);
		virtual Socket & getImpl();
	};

	/**
	 * @brief ServerSocket
	 */
	class ServerSocket : public OS::SocketOptions {
	private:
		ServerSocket * serverSocketImpl;
	public:
		ServerSocket();
		ServerSocket(int port);
		ServerSocket(const OS::InetAddress & bindAddr);
		virtual ~ServerSocket();

		virtual void bind();
		virtual void listen(int queueLimit);
		virtual Socket * accept();
		virtual void close();

	protected:
		virtual void createImpl();
		virtual void createImpl(int port);
		virtual void createImpl(const OS::InetAddress & bindAddr);
		virtual ServerSocket & getImpl();
	};
}

#endif