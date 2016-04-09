#include <iostream>
#include <liboslayer/os.hpp>
#include <liboslayer/DatagramSocket.hpp>

using namespace std;
using namespace OS;

/**
 * @brief UDP multicast Server
 */
class UDPMulticastServer : public Thread {
private:
	MulticastSocket * server;
	string group;
    int port;
    int cnt;
public:
    UDPMulticastServer(string group, int port) : group(group), port(port), cnt(0) {}
    virtual ~UDPMulticastServer() {}

    virtual void run() {
        Selector selector;
        server = new MulticastSocket(port);
		server->setReuseAddr(true);
        server->joinGroup(group.c_str());

        server->registerSelector(selector, Selector::READ);
		

        while (!interrupted()) {
            if (selector.select(10) > 0) {

				if (selector.isSelected(server->getFd())) {
					char buffer[1024] = {0,};
					DatagramPacket packet(buffer, sizeof(buffer));
					int len = server->recv(packet);
					if (len > 0) {
						InetAddress remoteAddr = packet.getRemoteAddr();
						cout << "[" << cnt++ << "] MCAST RECV (" << remoteAddr.getHost() << ":" << remoteAddr.getPort() << ") :" << packet.getData() << endl;
						send(remoteAddr.getHost().c_str(), remoteAddr.getPort(), "Welcome!!");
					}
				}
            }
        }
        server->close();
    }

	void send(const char * host, int port, const char * msg) {
		int ret;
		char buffer[1024] = {0,};
		DatagramPacket packet(buffer, sizeof(buffer), host, port);
		packet.write(msg);
		ret = server->send(packet);
	}
};

/**
 * @brief UDP Server
 */
class UDPServer : public Thread {
private:
	DatagramSocket * server;
    int port;
    int cnt;
public:
    UDPServer(int port) : port(port), cnt(0) {}
    virtual ~UDPServer() {}

    virtual void run() {
        Selector selector;
        server = new DatagramSocket(port);

        server->registerSelector(selector, Selector::READ);

        while (!interrupted()) {
            if (selector.select(10) > 0) {

				if (selector.isSelected(server->getFd())) {
					char buffer[1024] = {0,};
					DatagramPacket packet(buffer, sizeof(buffer));
					int len = server->recv(packet);
					if (len > 0) {
						cout << "[" << cnt++ << "] RECV: " << buffer << endl;
					}
				}
            }
        }
        server->close();
    }


	void send(const char * host, int port, const char * msg) {
		int ret;
		char buffer[1024] = {0,};
		DatagramPacket packet(buffer, sizeof(buffer), host, port);
		packet.write(msg);
		ret = server->send(packet);
	}
};


size_t readline(char * buffer, size_t max) {
	if (fgets(buffer, (int)max - 1, stdin)) {
		buffer[strlen(buffer) - 1] = 0;
		return strlen(buffer);
	}
	return 0;
}

void send(const char * host, int port, const char * msg) {
	DatagramSocket socket;
	int ret;
	char buffer[1024] = {0,};
	DatagramPacket packet(buffer, sizeof(buffer), host, port);
	packet.write(msg);
	ret = socket.send(packet);
}

int main(int argc, char * args[]) {

	UDPMulticastServer mcastServer("239.255.255.250", 1900);
	mcastServer.start();

	UDPServer udpServer(12345);
	udpServer.start();

	while (1) {
		char buffer[1024] = {0,};
		int len = readline(buffer, sizeof(buffer));
		if (len > 0) {
			if (!strcmp(buffer, "q")) {
				break;
			}
			if (!strcmp(buffer, "s")) {
				udpServer.send("239.255.255.250", 1900, "M-SEARCH * HTTP/1.1\r\n"
					"ST: upnp:rootdevice\r\n"
					"MAN: \"ssdp:discover\"\r\n"
					"HOST: 239.255.255.250:1900\r\n"
					"MX: 10\r\n"
					"Content-Length: 0\r\n"
					"\r\n");
			}
		}
	}

	mcastServer.interrupt();
	udpServer.interrupt();

	mcastServer.join();
	udpServer.join();

	return 0;
}
