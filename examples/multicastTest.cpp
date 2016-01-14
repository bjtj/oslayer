#include <liboslayer/os.hpp>
#include <liboslayer/DatagramSocket.hpp>

using namespace OS;

int main(int argc, char * args[]) {

	std::vector<InetAddress> addrs = Network::getAllInetAddress();
	for (size_t i = 0; i < addrs.size(); i++) {
		printf(" - %s\n", addrs[i].getHost().c_str());
	}

	DatagramSocket sock;

	SOCK_HANDLE s = sock.getSocket();

	unsigned long optval;
	socklen_t optlen = sizeof(optval);
	getsockopt(s, IPPROTO_IP, IP_MULTICAST_IF, (char*)&optval, &optlen);
    //SocketUtil::setSocketOption(s, IPPROTO_IP, IP_MULTICAST_IF, (const char*)&optval, optlen);

	printf("IP_MULTICAST_IF: %ld\n", optval);

	char buffer[1024] = {0,};
	DatagramPacket packet(buffer, sizeof(buffer), "239.255.255.250", 1900);
	packet.write("M-SEARCH * HTTP/1.1\r\n"
		"Host: 239.255.255.250:1900\r\n"
		"ST: upnp:rootdevice\r\n"
		"Man: \"ssdp:discover\"\r\n"
		"MX: 3\r\n"
		"USER-AGENT: Cross/0.1 UPnP/1.1 MyApp/0.1\r\n"
		"\r\n");

	for (size_t i = 0; i < addrs.size(); i++) {

		if (addrs[i].inet4()) {
			printf("setMulticastInteface/host: %s\n", addrs[i].getHost().c_str());
			sock.setMulticastInterface(addrs[i].getHost());
			sock.send(packet);
		}
	}

	for (int i = 0; i < 15; i++) {
		packet.clear();
		sock.recv(packet);
		printf("%s", packet.getData());
	}
	
    printf("Press any key to quit...\n");
    
	getchar();

	return 0;
}
