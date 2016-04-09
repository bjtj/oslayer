#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>

class ServerSocketThread : public OS::Thread {
private:
	int port;
public:
	ServerSocketThread(int port) : port(port) {
	}
	virtual ~ServerSocketThread() {
	}
	virtual void run() {
		
		OS::ServerSocket server(port);

		server.setReuseAddr(true);
		server.bind();
		server.listen(5);

		OS::Selector selector;
		server.registerSelector(selector, OS::Selector::READ);

		OS::InetAddress serverAddr = server.getLocalInetAddress();
		printf("Listen %s:%d\n", serverAddr.getHost().c_str(), serverAddr.getPort());

		while (!interrupted()) {

			if (selector.select(1000) > 0) {

				OS::Socket * client = server.accept();
				if (client) {

					printf("accepted\n");

					OS::InetAddress localAddr = client->getLocalInetAddress();
					OS::InetAddress remoteAddr = client->getRemoteInetAddress();
					printf("RECV FROM: %s:%d via %s:%d\n", remoteAddr.getHost().c_str(), remoteAddr.getPort(), localAddr.getHost().c_str(), localAddr.getPort());

					client->send("Hello", 5);
					char buffer[1024] = {0,};
					client->recv(buffer, sizeof(buffer));
					printf("RECV FROM CLIENT: %s\n", buffer);
					client->close();
					delete client;
				}
			}			
			printf("...\n");
		}

		server.close();
	}
};

void s_server_client_test(int port) {
	
	ServerSocketThread server(port);

	server.start();

	OS::idle(1000);

	OS::Socket client(OS::InetAddress("127.0.0.1", port));
	client.connect();
	char buffer[1024] = {0,};
	client.recv(buffer, sizeof(buffer) - 1);
	printf("RECV FROM SERVER: %s\n", buffer);
	client.send("bye~", 4);

	client.close();
}

void s_client_test() {

	OS::Socket sock(OS::InetAddress("www.google.com", 80));

	sock.connect();
	std::string packet = "GET / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";
	sock.send(packet.c_str(), packet.size());

	char buffer[4096] = {0,};
	sock.recv(buffer, sizeof(buffer));

	printf("RECV: %s\n", buffer);
	sock.close();
}

void s_client_connect_timeout_test() {

	OS::Socket sock(OS::InetAddress("www.google.com", 80));

	sock.connect(5000);
	printf("[connected]\n");

	std::string packet = "GET / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";
	sock.send(packet.c_str(), packet.size());

	char buffer[4096] = { 0, };
	sock.recv(buffer, sizeof(buffer));
	printf("RECV: %s\n", buffer);
	sock.close();
}

int main(int argc, char * args[]) {

	//s_client_test();
	s_server_client_test(9999);

	// try {
	// 	s_client_connect_timeout_test();
	// } catch (OS::IOException e) {
	// 	printf("Error - %s\n", e.what());
	// }
	
	getchar();

	return 0;
}
