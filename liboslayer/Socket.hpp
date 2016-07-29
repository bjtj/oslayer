#ifndef __SOCKET_HPP__
#define __SOCKET_HPP__

#include "os.hpp"

namespace OS {

	/**
	 * @brief Socket
	 */
	class Socket : public SocketOptions, public Selectable {
	private:
		Socket * socketImpl;
	public:
		Socket();
		Socket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		Socket(const InetAddress & remoteAddr);
		virtual ~Socket();

		virtual SOCK_HANDLE getSocket();
		virtual int getFd();
		virtual void setRemoteAddress(const OS::InetAddress & remoteAddr);
		virtual void negotiate();
		virtual void connect();
		virtual void connect(unsigned long timeout);
		virtual void disconnect();
		virtual void close();
		virtual bool isClosed();
		virtual int recv(char * buffer, size_t size);
		virtual int send(const char * data, size_t size);
		virtual InetAddress getLocalInetAddress();
		virtual InetAddress getRemoteInetAddress();
		virtual void setRecvTimeout(unsigned long timeout);
		virtual unsigned long getRecvTimeout();

	protected:
		virtual void createImpl();
		virtual void createImpl(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		virtual void createImpl(const InetAddress & remoteAddr);
		virtual Socket & getImpl();
	};

	/**
	 * @brief ServerSocket
	 */
	class ServerSocket : public SocketOptions, public Selectable {
	private:
		ServerSocket * serverSocketImpl;
	public:
		ServerSocket();
		ServerSocket(int port);
		ServerSocket(const InetAddress & bindAddr);
		virtual ~ServerSocket();

		virtual SOCK_HANDLE getSocket();
		virtual int getFd();
		virtual void bind();
		virtual void listen(int backlog);
		virtual Socket * accept();
		virtual SOCK_HANDLE accept(OS::SocketAddress & addr);
		virtual void close();
		virtual bool isClosed();
		virtual InetAddress getLocalInetAddress();

	protected:
		virtual void createImpl();
		virtual void createImpl(int port);
		virtual void createImpl(const InetAddress & bindAddr);
		virtual ServerSocket & getImpl();
	};
}

#endif
