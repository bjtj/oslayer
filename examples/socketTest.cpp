#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>

using namespace XOS;

class ServerSocketThread : public OS::Thread {
private:
public:
	ServerSocketThread() {
	}
	virtual ~ServerSocketThread() {
	}
	virtual void run() {
		ServerSocket server(8080);

		server.setResuseAddr(true);
		server.bind();
		server.listen(5);

		Socket * client = server.accept();
		if (client) {
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

	Socket client(OS::InetAddress("127.0.0.1", 8080));
	char buffer[1024] = {0,};
	client.recv(buffer, sizeof(buffer));
	printf("RECV FROM SERVER: %s\n", buffer);
	client.send("bye~", 4);

	client.close();
}

void s_client_test() {

	Socket sock;

	sock.connect(OS::InetAddress("www.google.com", 80));
	std::string packet = "GET / HTTP/1.1\r\nContent-Length: 0\r\n\r\n";
	sock.send(packet.c_str(), packet.size());

	char buffer[4096] = {0,};
	sock.recv(buffer, sizeof(buffer));

	printf("RECV: %s\n", buffer);
	sock.close();
}

int main(int argc, char * args[]) {

	//s_client_test();
	s_server_client_test();

	getchar();

	return 0;
}