#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Thread.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Text.hpp>

using namespace std;
using namespace osl;


/**
 * 
 */
class EchoServer : public Thread {
private:
	int _port;
	bool _verbose;
public:
    EchoServer(int port, bool verbose) : _port(port), _verbose(verbose) {
	}
    virtual ~EchoServer() {
	}
	virtual void run() {
		ServerSocket socket(_port);
		socket.setReuseAddr(true);
		socket.bind();
		socket.listen(50);
		if (_verbose) {
			printf("[listen...] port : %d\n", _port);
		}
		while (!interrupted()) {
			AutoRef<Socket> client(socket.accept());
			char buffer[1024] = {0,};
			try {
				int len = client->recv(buffer, sizeof(buffer));
				if (_verbose) {
					printf("RECV: %.*s\n", len, buffer);
				}
				client->send(buffer, len);
				client->close();
			} catch (IOException e) {
				printf("[ignore] exception - %s\n", e.what());
			}
		}
		socket.close();
	}
};

int main(int argc, char *argv[])
{
	System::getInstance()->ignoreSigpipe();
	int port = 9000;
	if (argc > 1) {
		port = Text::toInt(argv[1]);
	}
	printf("[port...] %d\n", port);

	EchoServer server(port, true);
	server.start();

	getchar();

	printf("[stop...]");
	server.interrupt();
	try {
		Socket c(InetAddress("127.0.0.1", port));
		c.connect();
		c.send("x", 1);
		c.close();
	} catch (IOException e) {
		printf("[ignore] exception - %s\n", e.what());
	}
	server.join();
	printf("[bye]\n");
	
    return 0;
}
