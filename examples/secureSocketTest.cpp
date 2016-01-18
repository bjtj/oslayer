#include <liboslayer/os.hpp>
#include <liboslayer/SecureSocket.hpp>

using namespace std;
using namespace OS;

class ServerSocketThread : public OS::Thread {
private:
	string certPath;
	string keyPath;
public:
	ServerSocketThread(const string & certPath, const string & keyPath) :
		certPath(certPath), keyPath(keyPath) {
	}
	virtual ~ServerSocketThread() {
	}
	virtual void run() {
		SecureServerSocket server(9000);

		server.loadCert(certPath, keyPath);

		server.setReuseAddr(true);
		server.bind();
		server.listen(5);

		InetAddress serverAddr = server.getLocalInetAddress();
		printf("Listen %s:%d\n", serverAddr.getHost().c_str(), serverAddr.getPort());

		while (!interrupted()) {
			Socket * client = server.accept();
			if (client) {

				InetAddress localAddr = client->getLocalInetAddress();
				InetAddress remoteAddr = client->getRemoteInetAddress();
				printf("Connected from: %s:%d via %s:%d\n",
					   remoteAddr.getHost().c_str(), remoteAddr.getPort(),
					   localAddr.getHost().c_str(), localAddr.getPort());

				bool done = false;
				char buffer[1024] = {0,};
				int readLen = 0;
				while (!done) {
					int len = client->recv(buffer + readLen, sizeof(buffer) - readLen);
					readLen += len;
					printf("RECV FROM CLIENT: %s\n", buffer);
					if (string(buffer).find("\r\n\r\n") != string::npos) {
						break;
					}
				}

				const char * packet = "HTTP/1.1 200 OK\r\nContent-Length:5\r\n\r\nhello";
				printf("write: %s\n", packet);
				int ret = client->send(packet, strlen(packet));
				printf("send result: %d\n", ret);

				printf("write done.. connection close\n");
				client->close();
			}
		}

		server.close();
	}
};

size_t readline(char * buffer, size_t max) {
    if (fgets(buffer, (int)max - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return strlen(buffer);
	}
    return 0;
}

int main(int argc, char *args[]) {

	char certPath[1024] = {0,};
	char keyPath[1024] = {0,};
	printf("cert path: ");
	readline(certPath, sizeof(certPath));
	printf("key path: ");
	readline(keyPath, sizeof(keyPath));

	ServerSocketThread server(certPath, keyPath);

	printf("start...\n");
	server.start();

	getchar();

	server.interrupt();
	
    return 0;
}
