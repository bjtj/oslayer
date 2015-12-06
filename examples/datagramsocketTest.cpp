#include <liboslayer/os.hpp>
#include <liboslayer/DatagramSocket.hpp>

using namespace std;
using namespace XOS;

class DatagramServerThread : public OS::Thread {
private:
	OS::Semaphore sem;
public:
	DatagramServerThread() : sem(1) {
		sem.wait();
	}
	virtual ~DatagramServerThread() {
	}
	void wait() {
		sem.wait();
	}
	void notify() {
		sem.post();
	}
	virtual void run() {
		DatagramSocket server(9000);

		notify();

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

	server.wait();

	DatagramSocket client;
	char buffer[1024] = {0,};
	OS::DatagramPacket packet(buffer, sizeof(buffer));
	packet.write("hello");
	packet.setRemoteAddr(OS::InetAddress("127.0.0.1", 9000));
	client.send(packet);

	client.close();
}

void s_multicast_test(const std::string & group) {

	MulticastSocket server(1900);

	server.joinGroup(group);

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

void s_send_multicast_test(int inetVersion, const std::string & group) {

	OS::InetAddress addr(0);
	addr.setInetVersion(inetVersion);
	DatagramSocket sender(addr);

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
	packet.setRemoteAddr(OS::InetAddress(group, 1900));
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

	s_datagram_server_client();
	//s_multicast_test("239.255.255.250");
	//s_multicast_test("[FF02::C]");
	//s_send_multicast_test(OS::InetAddress::InetVersion::INET4, "239.255.255.250");
	//s_send_multicast_test(OS::InetAddress::InetVersion::INET6, "[FF02::C]");
	
	getchar();

	return 0;
}