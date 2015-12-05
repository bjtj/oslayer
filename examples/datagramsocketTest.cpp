#include <liboslayer/os.hpp>
#include <liboslayer/DatagramSocket.hpp>

using namespace std;
using namespace XOS;

class DatagramServerThread : public OS::Thread {
private:
public:
	DatagramServerThread() {
	}
	virtual ~DatagramServerThread() {
	}
	virtual void run() {
		DatagramSocket server(9000);

		char buffer[1024] = {0,};
		OS::DatagramPacket packet(buffer, sizeof(buffer));
		server.recv(packet);

		printf("Recv from: %s:%d\n", packet.getRemoteAddr().getHost().c_str(), packet.getRemoteAddr().getPort());
		printf("%s\n", packet.getData());

		printf("done\n");
	}
};

void s_datagram_server_client() {

	DatagramServerThread server;

	server.start();

	OS::idle(100);

	DatagramSocket client;
	char buffer[1024] = {0,};
	OS::DatagramPacket packet(buffer, sizeof(buffer));
	packet.write("hello");
	packet.setRemoteAddr(OS::InetAddress("127.0.0.1", 9000));
	client.send(packet);

	client.close();
}

void s_multicast_test() {

	MulticastSocket server(1900);

	server.joinGroup("239.255.255.250");

	char buffer[1024] = {0,};
	OS::DatagramPacket packet(buffer, sizeof(buffer));

	while (1) {
		packet.clear();
		server.recv(packet);
		std::string host = packet.getRemoteAddr().getHost();
		int port = packet.getRemoteAddr().getPort();

		printf("RECV FROM: %s:%d\n", host.c_str(), port);
		printf("%s\n", packet.getData());
	}
	
}

void s_send_multicast_test() {
	DatagramSocket sender;

	std::string content = "M-SEARCH * HTTP/1.1\r\n"
		"Host: 239.255.255.250:1900\r\n"
		"ST: upnp:rootdevice\r\n"
		"Man: \"ssdp:discover\"\r\n"
		"MX: 3\r\n"
		"USER-AGENT: Android/23 UPnP/1.1 UPnPTool/1.4.3\r\n"
		"\r\n";

	char buffer[1024] = {0,};
	OS::DatagramPacket packet(buffer, sizeof(buffer));
	packet.write(content);
	packet.setRemoteAddr(OS::InetAddress("239.255.255.250", 1900));
	sender.send(packet);
	
	while (1) {

		packet.clear();
		sender.recv(packet);
		std::string host = packet.getRemoteAddr().getHost();
		int port = packet.getRemoteAddr().getPort();

		printf("RECV FROM: %s:%d\n", host.c_str(), port);
		printf("%s\n", packet.getData());
	}
}

int main(int argc, char * args[]) {

	//s_datagram_server_client();
	//s_multicast_test();
	s_send_multicast_test();
	
	getchar();

	return 0;
}