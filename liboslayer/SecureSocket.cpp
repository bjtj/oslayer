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
	 * https://www.openssl.org/docs/manmaster/crypto/OpenSSL_add_all_algorithms.html
	 *  - OpenSSL_add_all_algorithms()
	 *  - EVP_cleanup()
	 * https://www.openssl.org/docs/manmaster/crypto/ERR_remove_state.html
	 *  - ERR_remove_thread_state()
	 */

	SecureContext::SecureContext() {
		SSL_library_init();
		SSL_load_error_strings();
		OpenSSL_add_all_algorithms();
	}
	SecureContext::~SecureContext() {
		EVP_cleanup();
		ERR_free_strings();
	}
	SecureContext & SecureContext::getInstance() {
		static SecureContext instance;
		return instance;
	}

	

	SecureSocket::SecureSocket(SSL_CTX * ctx, SOCK_HANDLE sock, struct sockaddr * addr, socklen_t addrlen) : Socket(sock, addr, addrlen) {

		SecureContext::getInstance();
        
        setSelectable(false);

		ssl = SSL_new(ctx);
		SSL_set_fd(ssl, sock); // TODO: READ THIS -> https://www.openssl.org/docs/manmaster/ssl/SSL_set_fd.html

		SSL_accept(ssl); // TODO: READ THIS -> https://www.openssl.org/docs/manmaster/ssl/SSL_accept.html
	}
	SecureSocket::~SecureSocket() {
	}
	int SecureSocket::recv(char * buffer, size_t size) {
		bool done = false;
		int len = 0;
		while (!done) {
			len = SSL_read(ssl, buffer, (int)size);
			int ssl_err = SSL_get_error(ssl, len);
			ERR_clear_error();
			switch (ssl_err) {
			case SSL_ERROR_NONE:
				return len;
			case SSL_ERROR_ZERO_RETURN:
				done = true;
				break;
			case SSL_ERROR_WANT_READ:
				printf("read again...\n");
				break;
			case SSL_ERROR_WANT_WRITE:
				printf("write??\n");
				break;
			default:
                throw IOException("SSL_read() error - " + string(ERR_error_string(ssl_err, NULL)), -1, 0);
			}
		}
		return len;
	}
	int SecureSocket::send(const char * data, size_t size) {
		bool done = false;
		int len = 0;
		while (!done) {
			len = SSL_write(ssl, data, (int)size);
			int ssl_err = SSL_get_error(ssl, len);
			ERR_clear_error();
			switch (ssl_err) {
			case SSL_ERROR_NONE:
				return len;
			case SSL_ERROR_WANT_READ:
				printf("read??...\n");
				break;
			case SSL_ERROR_WANT_WRITE:
				printf("write again...\n");
				break;
			default:
				throw IOException("SSL_write() error - " + string(ERR_error_string(ssl_err, NULL)), -1, 0);
			}
		}
		return len;
	}
	void SecureSocket::close() {
		if (ssl) {
            ERR_remove_state(0);
			//ERR_remove_thread_state(NULL);
			CRYPTO_cleanup_all_ex_data();
			SSL_free(ssl);
			ssl = NULL;
		}
		Socket::close();
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
		
		method = TLSv1_server_method();
		ctx = SSL_CTX_new(method);
		if (ctx == NULL) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_CTX_new() failed", -1, 0);
		}
	}
	void SecureServerSocket::loadCert(const string & certPath, const string & keyPath) {
		if (SSL_CTX_use_certificate_file(ctx, certPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_CTX_use_certificate_file() failed");
		}
		if (SSL_CTX_use_PrivateKey_file(ctx, keyPath.c_str(), SSL_FILETYPE_PEM) <= 0) {
			ERR_print_errors_fp(stderr);
			throw IOException("SSL_CTX_use_PrivateKey_file() failed");
		}
		if (!SSL_CTX_check_private_key(ctx)) {
			fprintf(stderr, "Private key does not match the public certificate\n");
			throw IOException("SSL_CTX_check_private_key() failed");
		}
	}
	Socket * SecureServerSocket::accept() {
		SocketAddress sa;
		SOCK_HANDLE client = ServerSocket::accept(sa);
		return new SecureSocket(ctx, client, sa.getAddr(), *sa.getAddrLen());
	}
	void SecureServerSocket::close() {
        ERR_remove_state(0);
		//ERR_remove_thread_state(NULL); // https://github.com/warmcat/libwebsockets/issues/186
									   // Replacement of ERR_remove_state()
		CRYPTO_cleanup_all_ex_data();
		SSL_CTX_free(ctx);
		ServerSocket::close();
	}
}

#endif
