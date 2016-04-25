#ifndef __SECURE_SOCKET_HPP__
#define __SECURE_SOCKET_HPP__

#include "Socket.hpp"

#if defined(USE_OPENSSL)

#include <openssl/ssl.h>
#include <openssl/err.h>

namespace OS {

	class SecureContext {
	private:
		SecureContext();
		SecureContext(const SecureContext & other);
		virtual ~SecureContext();
	public:
		static SecureContext & getInstance();
	};

	class SecureSocket : public Socket {
	private:
		SSL * ssl;
	public:
		SecureSocket(SSL_CTX * ctx, SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		virtual ~SecureSocket();
		virtual int recv(char * buffer, size_t size);
		virtual int send(const char * data, size_t size);
		virtual void close();
	};
	
	class SecureServerSocket : public ServerSocket {
	private:
		const SSL_METHOD * method;
		SSL_CTX * ctx;
	public:
		SecureServerSocket();
		SecureServerSocket(int port);
		SecureServerSocket(const InetAddress & bindAddr);
		void initOpenSSL();
		virtual ~SecureServerSocket();
		void loadCert(const std::string & certPath, const std::string & keyPath);
		virtual Socket * accept();
		virtual void close();
	};
}

#endif // OPENSSL capability

#endif
