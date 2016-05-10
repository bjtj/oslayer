#include "SecureSocket.hpp"

#if defined(USE_OPENSSL)

namespace OS {

	using namespace std;

	/**
	 * References
	 * ==========
 	 * https://wiki.openssl.org/index.php/Library_Initialization
	 *  - initialization
	 *  - cleanup
	 *  !) CRYPTO_cleanup_all_ex_data and ERR_remove_state should be called on each thread,
	 *  !) and not just the main thread
	 *  ** CRYPTO_cleanup_all_ex_data() is thread un-safe
	 *   > http://stackoverflow.com/a/21533000/5676460
	 *    . In most cases the cause of an apparent memory leak is an OpenSSL internal table that is
	 *      allocated when an application starts up. Since such tables do not grow in size over time they are
	 *      harmless.
	 * https://www.openssl.org/docs/manmaster/crypto/OpenSSL_add_all_algorithms.html
	 *  - OpenSSL_add_all_algorithms()
	 *  - EVP_cleanup()
	 * https://www.openssl.org/docs/manmaster/crypto/ERR_remove_state.html
	 *  - ERR_remove_thread_state()
	 * http://fm4dd.com/openssl/sslconnect.htm
	 *  - ssl client
	 */
	
	SecureContext::SecureContext() {
		SSL_library_init();
		SSL_load_error_strings();
		OpenSSL_add_all_algorithms();
	}
	SecureContext::~SecureContext() {
		CRYPTO_cleanup_all_ex_data();
		EVP_cleanup();
		ERR_free_strings();
	}
	SecureContext & SecureContext::getInstance() {
		static SecureContext instance;
		return instance;
	}

	string SecureContext::getOpenSSLVersion() {
		return string(OPENSSL_VERSION_TEXT);
	}

	/* created by server accept */
	SecureSocket::SecureSocket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : Socket(sock, addr, addrlen), ctx(NULL), ssl(NULL), peerCert(NULL), verifier(NULL), peerCertRequired(false), needHandshake(true) {
		SecureContext::getInstance();
        setSelectable(false);
		ctx = SSL_CTX_new(TLSv1_server_method());
		if (!ctx) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_CTX_new() failed");
		}
		ssl = SSL_new(ctx);
		if (!ssl) {
			throw IOException("SSL_new() failed");
		}
		
		SSL_set_accept_state(ssl);
		
		if (SSL_set_fd(ssl, sock) != 1) {
			throw IOException("SSL_set_fd() failed");
		}
	}

	SecureSocket::SecureSocket(SSL_CTX * ctx, SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : Socket(sock, addr, addrlen), ctx(NULL), ssl(NULL), peerCert(NULL), verifier(NULL), peerCertRequired(false), needHandshake(true) {
        setSelectable(false);
		if (!ctx) {
			throw IOException("SSL_CTX is null");
		}
		ssl = SSL_new(ctx);
		if (!ssl) {
			throw IOException("SSL_new() failed");
		}
		
		SSL_set_accept_state(ssl);
		
		if (SSL_set_fd(ssl, sock) != 1) {
			throw IOException("SSL_set_fd() failed");
		}
	}

	/* created to connect (client mode) */
	SecureSocket::SecureSocket(const InetAddress & remoteAddr) : Socket(remoteAddr), ctx(NULL), ssl(NULL), peerCert(NULL), verifier(NULL), peerCertRequired(true), needHandshake(false)
 {
		SecureContext::getInstance();
		ctx = SSL_CTX_new(SSLv23_client_method());
		if (!ctx) {
			throw IOException("SSL_CTX_new() failed");
		}
		SSL_CTX_set_options(ctx, SSL_OP_NO_SSLv2);
		ssl = SSL_new(ctx);
		if (!ssl) {
			throw IOException("SSL_new() failed");
		}
		
		SSL_set_connect_state(ssl);

		if (SSL_set_fd(ssl, getSocket()) != 1) {
			throw IOException("SSL_set_fd() failed");
		}
	}
	
	SecureSocket::~SecureSocket() {
	}

	void SecureSocket::loadCert(const string & certPath, const string & keyPath) {
		if (SSL_use_certificate_file(ssl, certPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			throw IOException("SSL_use_certificate_file() failed");
		}
		if (SSL_use_PrivateKey_file(ssl, keyPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_use_PrivateKey_file() failed");
		}
		if (!SSL_check_private_key(ssl)) {
			throw IOException("SSL_check_private_key() failed");
		}
	}

	void SecureSocket::negotiate() {
		if (needHandshake) {
			needHandshake = false;
			handshake();
		}
	}

	void SecureSocket::connect() {
		Socket::connect();
		int ret = SSL_connect(ssl);
		int ssl_err = SSL_get_error(ssl, ret);
		ERR_clear_error();
		if (ret != 1) {
			throw IOException("SSL_connect() error - '" + getErrorString(ssl_err) + "'");
		}
		verify();
	}

	void SecureSocket::handshake() {
		int ret = SSL_accept(ssl);
		int ssl_err = SSL_get_error(ssl, ret);
		ERR_clear_error();
		if (ret == 0) {
			throw IOException("handshake succeeded but shutdown");
		} else if (ret < 0) {
			throw IOException("handshake failed - '" + getErrorString(ssl_err) + "'");
		}
		verify();
	}

	void SecureSocket::verify() {
		if ((peerCert = SSL_get_peer_certificate(ssl)) == NULL) {
			if (peerCertRequired) {
				throw IOException("No peer certificate - remote server must have a certificate");
			} else {
				return;
			}
		}
		long err = SSL_get_verify_result(ssl);
		if (verifier) {
			if (verifier->onVerify(VerifyError(err), X509Certificate(peerCert)) == false) {
				throw IOException("Peer certificate verification failed (by custom verifier)");
			}
		} else if (err != X509_V_OK) {
			throw IOException("Peer certificate verification failed"); 
		}
	}
	
	int SecureSocket::recv(char * buffer, size_t size) {
		
		int len = SSL_read(ssl, buffer, (int)size);
		if (len <= 0) {
			if (SSL_get_shutdown(ssl) & SSL_RECEIVED_SHUTDOWN) {
				SSL_shutdown(ssl);
			} else {
				SSL_clear(ssl);
			}
		}
			
		if (len == 0) {
			throw IOException("SecureSocket::recv() - normal close", len, 0);
		} else if (len < 0) {
			throw IOException("SecureSocket::recv() - something wrong", len, 0);
		}
		return len;
	}
	int SecureSocket::send(const char * data, size_t size) {
		
		int len = SSL_write(ssl, data, (int)size);

		if (len == 0) {
			SSL_shutdown(ssl);
			throw IOException("SecureSocket::send() - maybe internal close", len, 0);
		} else if (len < 0) {
			SSL_clear(ssl);
			throw IOException("SecureSocket::send() - something wrong", len, 0);
		}
		
		return len;
	}
	void SecureSocket::close() {

		if (peerCert) {
			X509_free(peerCert);
			peerCert = NULL;
		}
		
		if (ssl) {
			SSL_free(ssl);
			ssl = NULL;
		}

		if (ctx) {
			SSL_CTX_free(ctx);
			ctx = NULL;
		}
		Socket::close();

#if OPENSSL_API_COMPAT < 0x10000000L
		ERR_remove_state(0);
#else
		ERR_remove_thread_state(NULL);
#endif
	}

	void SecureSocket::setVerifier(CertificateVerifier * verifier) {
		this->verifier = verifier;
	}

	void SecureSocket::setPeertCertRequired(bool required) {
		this->peerCertRequired = required;
	}
	string SecureSocket::getErrorString(unsigned long err) {
		char buffer[256] = {0,};
		ERR_error_string_n(err, buffer, sizeof(buffer));
		return string(buffer);
	}

	

	SecureServerSocket::SecureServerSocket() {
		initOpenSSL();
	}
	SecureServerSocket::SecureServerSocket(int port) : ServerSocket(port) {
		initOpenSSL();
	}
	SecureServerSocket::SecureServerSocket(const InetAddress & bindAddr)
		: ServerSocket(bindAddr) {
		
		initOpenSSL();
	}
	SecureServerSocket::~SecureServerSocket() {
	}
	void SecureServerSocket::initOpenSSL() {
		SecureContext::getInstance();
        setSelectable(false);
		ctx = SSL_CTX_new(TLSv1_server_method());
		if (!ctx) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_CTX_new() failed");
		}
	}
	void SecureServerSocket::loadCert(const string & certPath, const string & keyPath) {
		if (SSL_CTX_use_certificate_file(ctx, certPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			throw IOException("SSL_CTX_use_certificate_file() failed");
		}
		if (SSL_CTX_use_PrivateKey_file(ctx, keyPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			throw IOException("SSL_CTX_use_PrivateKey_file() failed");
		}
		if (!SSL_CTX_check_private_key(ctx)) {
			throw IOException("SSL_CTX_check_private_key() failed");
		}
	}
	
	Socket * SecureServerSocket::accept() {
		SocketAddress sa;
		SOCK_HANDLE client = ServerSocket::accept(sa);
		return new SecureSocket(ctx, client, sa.getAddr(), *sa.getAddrLen());
	}
	
	void SecureServerSocket::close() {
		SSL_CTX_free(ctx);
		ServerSocket::close();
#if OPENSSL_API_COMPAT < 0x10000000L
        ERR_remove_state(0);
#else
		ERR_remove_thread_state(NULL);
#endif
	}

	void SecureServerSocket::setVerifier(CertificateVerifier * verifier) {
		this->verifier = verifier;
	}
	

	/**
	 * certifcate
	 */

	Certificate::Certificate() {
	}
	Certificate::~Certificate() {
	}


	X509Certificate::X509Certificate(X509 * cert) : cert(cert) {
	}
	X509Certificate::~X509Certificate() {
	}
	string X509Certificate::getSubjectName() const {
		char buffer[256] = {0,};
		X509_NAME_oneline(X509_get_subject_name(cert), buffer, sizeof(buffer));
		return string(buffer);
	}
	string X509Certificate::getIssuerName() const {
		char buffer[256] = {0,};
		X509_NAME_oneline(X509_get_issuer_name(cert), buffer, sizeof(buffer));
		return string(buffer);
	}


	VerifyError::VerifyError(long code) : code(code) {
	}
	VerifyError::~VerifyError() {
	}
	bool VerifyError::okay() const {
		return code == X509_V_OK;
	}
	bool VerifyError::failed() const {
		return code != X509_V_OK;
	}
	

	CertificateVerifier::CertificateVerifier() {
	}
	CertificateVerifier::~CertificateVerifier() {
	}
}

#endif
