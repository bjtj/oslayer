#include "SecureSocket.hpp"

#if defined(USE_OPENSSL)

namespace osl {

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


	/**
	 * # OpenSSL 1.1.0 changes
	 * 
	 * * https://wiki.openssl.org/index.php/OpenSSL_1.1.0_Changes
	 *
	 * ## Backward compatibility
	 * 
	 * ```
	 * #if OPENSSL_VERSION_NUMBER < 0x10100000L
	 * ```
	 */


	static vector<AutoRef<Semaphore> > locks;

	static unsigned long thread_id() {
#if defined(USE_PTHREAD)
		return (unsigned long)pthread_self();
#endif
	}

	static void locking_callback(int mode, int type, const char * file, int line) {
		if (mode & CRYPTO_LOCK) {
			locks[type]->wait();
		} else {
			locks[type]->post();
		}
	}

	static void setup_thread_support() {
		for (int i = 0; i < CRYPTO_num_locks(); i++) {
			locks.push_back(AutoRef<Semaphore>(new Semaphore(1)));
		}

#if defined(USE_PTHREAD)
		CRYPTO_set_id_callback(thread_id);
#endif
		CRYPTO_set_locking_callback(&locking_callback);
	}

	static void clear_thread_support() {
	}
	
	SecureContext::SecureContext() {
		SSL_library_init();
		SSL_load_error_strings();
		OpenSSL_add_all_algorithms();

		setup_thread_support();
	}
	SecureContext::~SecureContext() {

		clear_thread_support();
		
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

	/**
	 * @brief secure server
	 */
	
	/* created by server accept */
	SecureSocket::SecureSocket(SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) :
		Socket(sock, addr, addrlen),
		ctx(NULL),
		ssl(NULL),
		peerCert(NULL),
		verifier(NULL),
		peerCertRequired(false),
		needHandshake(true),
		recvTimeout(0),
		read_blocked(false),
		read_blocked_on_write(false),
		write_blocked_on_read(false)
	{
		SecureContext::getInstance();
#if OPENSSL_VERSION_NUMBER < 0x10100000L
		ctx = SSL_CTX_new(TLSv1_server_method());
#else
		// version specific methods are deprecated in openssl 1.1.0
		ctx = SSL_CTX_new(TLS_server_method());
#endif
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

	SecureSocket::SecureSocket(SSL_CTX * ctx, SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) :
		Socket(sock, addr, addrlen),
		ctx(NULL),
		ssl(NULL),
		peerCert(NULL),
		verifier(NULL),
		peerCertRequired(false),
		needHandshake(true),
		read_blocked(false),
		read_blocked_on_write(false),
		write_blocked_on_read(false)
	{
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
	SecureSocket::SecureSocket(const InetAddress & remoteAddr) :
		Socket(remoteAddr),
		ctx(NULL),
		ssl(NULL),
		peerCert(NULL),
		verifier(NULL),
		peerCertRequired(true),
		needHandshake(false),
		read_blocked(false),
		read_blocked_on_write(false),
		write_blocked_on_read(false)
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
	    close();
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

	void SecureSocket::connect(unsigned long timeout) {
		Socket::connect(timeout);
		int ret = SSL_connect(ssl);
		int ssl_err = SSL_get_error(ssl, ret);
		ERR_clear_error();
		if (ret != 1) {
			throw IOException("SSL_connect() error - '" + getErrorString(ssl_err) + "'");
		}
		verify();
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
		if (!verifier.nil()) {
			if (verifier->onVerify(VerifyError(err), X509Certificate(peerCert)) == false) {
				throw IOException("Peer certificate verification failed (by custom verifier)");
			}
		} else if (err != X509_V_OK) {
			throw IOException("Peer certificate verification failed"); 
		}
	}

	bool SecureSocket::isReadable(Selector & selector) {
		return ((selector.isReadable(getFd()) && !write_blocked_on_read) ||
				(read_blocked_on_write && selector.isWritable(getFd())));
	}
	
	bool SecureSocket::isWritable(Selector & selector) {
		return ((selector.isWritable(getFd())) ||
				(write_blocked_on_read && selector.isReadable(getFd())));
	}

	int SecureSocket::pending() {
		return (read_blocked ? 0 : SSL_pending(ssl));
	}
	
	int SecureSocket::recv(char * buffer, size_t size) {
		read_blocked_on_write = false;
		read_blocked = false;
		int ret = SSL_read(ssl, buffer, (int)size);
		switch (SSL_get_error(ssl, ret)) {
		case SSL_ERROR_NONE:
			return ret;
		case SSL_ERROR_ZERO_RETURN:
			SSL_shutdown(ssl);
			throw IOException("normal close", ret, 0);
		case SSL_ERROR_WANT_READ:
			read_blocked = true;
			return 0;
		case SSL_ERROR_WANT_WRITE:
			read_blocked_on_write = true;
			return 0;
		default:
			throw IOException("SSL error - SSL_read()");
		}
	}
	
	int SecureSocket::send(const char * data, size_t size) {
		write_blocked_on_read = false;
		int ret = SSL_write(ssl, data, (int)size);
		switch(SSL_get_error(ssl, ret)) {
		case SSL_ERROR_NONE:
			return ret;
		case SSL_ERROR_WANT_WRITE:
			return 0;
		case SSL_ERROR_WANT_READ:
			write_blocked_on_read = true;
			return 0;
		default:
			throw IOException("SSL error - SSL_write()");
		}
	}

	void SecureSocket::setRecvTimeout(unsigned long timeout) {
		this->recvTimeout = timeout;
	}
	
	unsigned long SecureSocket::setRecvTimeout() {
		return recvTimeout;
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
	}

	void SecureSocket::setVerifier(AutoRef<CertificateVerifier> verifier) {
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
	

	/**
	 * @brief secure server socket
	 */
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
	    close();
	}
	void SecureServerSocket::initOpenSSL() {
		SecureContext::getInstance();
#if OPENSSL_VERSION_NUMBER < 0x10100000L
		ctx = SSL_CTX_new(TLSv1_server_method());
#else
		// version specific methods are deprecated in openssl 1.1.0
		ctx = SSL_CTX_new(TLS_server_method());
#endif
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
	}

	void SecureServerSocket::setVerifier(AutoRef<CertificateVerifier> verifier) {
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

#else

namespace osl {
	// no ssl
	SecureServerSocket::SecureServerSocket() {
		throw Exception("ssl not available");
	}
	SecureServerSocket::~SecureServerSocket() {
	}
}

#endif
