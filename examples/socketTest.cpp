#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>

using namespace std;
using namespace XOS;

class ServerThread : public OS::Thread {
private:
public:
	ServerThread() {
	}
	virtual ~ServerThread() {
	}
	virtual void run() {
		DatagramSocket server(9000);

		char buffer[1024] = {0,};
		OS::DatagramPacket packet(buffer, sizeof(buffer));
		server.recv(packet);

		printf("RECV: %s\n", packet.getData());

		printf("done\n");
	}
};

void s_server_client() {
	ServerThread server;

	server.start();

	OS::idle(100);

	DatagramSocket client;
	char message[1024] = {0,};
	snprintf(message, sizeof(message), "hello");
	OS::DatagramPacket packet(message, sizeof(message));
	packet.setLength(strlen(message));
	OS::InetAddress remoteAddr("127.0.0.1", 9000);
	packet.setRemoteAddr(remoteAddr);
	client.send(packet);
}

void s_datagram_packet() {

	DatagramSocket client;

	char buffer[1024] = {0,};
	snprintf(buffer, sizeof(buffer), "hello");
	OS::DatagramPacket packet(buffer, sizeof(buffer));
	packet.setLength(strlen(buffer));

	OS::InetAddress addr("127.0.0.1", 9000);
	packet.setRemoteAddr(addr);

	printf("%s\n", packet.getData());

	client.send(packet);
}



int main(int argc, char * args[]) {

	s_server_client();
	//s_datagram_packet();
	
	getchar();

	return 0;
}