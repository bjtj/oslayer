#ifndef __SECURE_SOCKET_HPP__
#define __SECURE_SOCKET_HPP__

#include "Socket.hpp"

#if defined(USE_OPENSSL)

#include <openssl/ssl.h>
#include <openssl/err.h>
#include <openssl/x509.h>

namespace OS {

	class Certificate;
	class X509Certificate;
	class VerifyError;
	class CertificateVerifier;

	/**
	 * @brief 
	 */
	class SecureContext {
	private:
		SecureContext();
		SecureContext(const SecureContext & other);
		virtual ~SecureContext();
	public:
		static SecureContext & getInstance();
	};

	/**
	 * @brief
	 */
	class SecureSocket : public Socket {
	private:
		SSL_CTX * ctx;
		SSL * ssl;
		X509 * peerCert;
		CertificateVerifier * verifier;
		bool peerCertRequired;
		bool needHandshake;
	public:

		SecureSocket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		SecureSocket(SSL_CTX * ctx, SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen);
		SecureSocket(const OS::InetAddress & remoteAddr);
		virtual ~SecureSocket();
		virtual void loadCert(const std::string & certPath, const std::string & keyPath);
		virtual void negotiate();
		virtual void connect();
		void handshake();
		void verify();
		virtual int recv(char * buffer, size_t size);
		virtual int send(const char * data, size_t size);
		virtual void close();
		void setVerifier(CertificateVerifier * verifier);
		void setPeertCertRequired(bool required);
	};

	/**
	 * @brief
	 */
	class SecureServerSocket : public ServerSocket {
	private:
		SSL_CTX * ctx;
		CertificateVerifier * verifier;
	public:
		SecureServerSocket();
		SecureServerSocket(int port);
		SecureServerSocket(const InetAddress & bindAddr);
		virtual ~SecureServerSocket();
		void initOpenSSL();
		void loadCert(const std::string & certPath, const std::string & keyPath);
		virtual Socket * accept();
		virtual void close();
		void setVerifier(CertificateVerifier * verifier);
	};


	/**
	 * @brief certificate
	 */
	class Certificate {
	public:
		Certificate();
		virtual ~Certificate();
		virtual std::string getSubjectName() const = 0;
		virtual std::string getIssuerName() const = 0;
	};

	/**
	 * @brief 
	 */
	class X509Certificate : public Certificate {
	private:
		X509 * cert;
	public:
		X509Certificate(X509 * cert);
		virtual ~X509Certificate();
		virtual std::string getSubjectName() const;
		virtual std::string getIssuerName() const;
	};

	/**
	 * @brief 
	 */
	class VerifyError {
	private:
		long code;
	public:
		VerifyError(long code);
		virtual ~VerifyError();
		bool okay() const;
		bool failed() const;
	};

	/**
	 * @brief 
	 */
	class CertificateVerifier {
	public:
		CertificateVerifier();
		virtual ~CertificateVerifier();
		virtual bool onVerify(const VerifyError & err, const Certificate & cert) = 0;
	};
}

#endif // OPENSSL capability

#endif
