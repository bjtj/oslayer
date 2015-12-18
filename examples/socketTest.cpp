#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>

class ServerSocketThread : public OS::Thread {
private:
public:
	ServerSocketThread() {
	}
	virtual ~ServerSocketThread() {
	}
	virtual void run() {
		OS::ServerSocket server(8080);

		server.setReuseAddr(true);
		server.bind();
		server.listen(5);

		OS::InetAddress serverAddr = server.getLocalInetAddress();
		printf("Listen %s:%d\n", serverAddr.getHost().c_str(), serverAddr.getPort());

		OS::Socket * client = server.accept();
		if (client) {

			OS::InetAddress localAddr = client->getLocalInetAddress();
			OS::InetAddress remoteAddr = client->getRemoteInetAddress();
			printf("RECV FROM: %s:%d via %s:%d\n", remoteAddr.getHost().c_str(), remoteAddr.getPort(), localAddr.getHost().c_str(), localAddr.getPort());

			client->send("hello", 5);
			char buffer[1024] = {0,};
			client->recv(buffer, sizeof(buffer));
			printf("RECV FROM CLIENT: %s\n", buffer);
			client->close();
		}

		server.close();
	}
};

void s_server_client_test() {
	ServerSocketThread server;

	server.start();

	OS::Socket client(OS::InetAddress("127.0.0.1", 8080));
	client.connect();
	char buffer[1024] = {0,};
	client.recv(buffer, sizeof(buffer));
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
	//s_server_client_test();

	try {
		s_client_connect_timeout_test();
	} catch (OS::IOException e) {
		printf("Error - %s\n", e.what());
	}
	
	getchar();

	return 0;
}