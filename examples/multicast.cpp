#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

void s_msearch(const string & st) {

	DatagramSocket sock(9000);

	sock.setReuseAddr();
	sock.joinGroup("239.255.255.250");

	string packet = "M-SEARCH * HTTP/1.1\r\n"
		"HOST: 239.255.255.250:1900\r\n"
		"MAN: \"ssdp:discovery\"\r\n"
		"MX: 3\r\n"
		"ST: " + st + "\r\n"
		"USER-AGENT: Cross/0.1 UPnP/1.1 App/0.1\r\n"
		"\r\n";
	sock.send("239.255.255.250", 1900, packet.c_str(), packet.length());

	while (1) {
		char buffer[1024] = {0,};
		DatagramPacket packet(buffer, sizeof(buffer));
		sock.recv(packet);

		printf("%s", packet.getData());
	}
}

void s_listen() {
	DatagramSocket sock(1900);

	sock.setReuseAddr();
	sock.joinGroup("239.255.255.250");

	while (1) {
		char buffer[1024] = {0,};
		DatagramPacket packet(buffer, sizeof(buffer));
		sock.recv(packet);

		printf("%s:%d\n", packet.getRemoteAddr().c_str(), packet.getRemotePort());
		printf("%s", packet.getData());
	}
}

int main(int argc, char * args[]) {

	//s_msearch("upnp:rootdevice");
	//s_msearch("ssdp:all");
	s_listen();
	
	return 0;
}