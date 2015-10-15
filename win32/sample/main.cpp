#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

class MyThread;
class SSDPServer;

static void thread();
static void ssdp();

int main(int argc, char * args[]) {

	ssdp();

	return 0;
}

class MyThread : public Thread {
private:
public:
	MyThread() {}
	virtual ~MyThread() {}

	virtual void run() {
		cout << "hello in thread" << endl;
	}
};

class SSDPServer : public Thread {
private:
	DatagramSocket * socket;
public:
	SSDPServer() {}
	virtual ~SSDPServer() {}

	virtual void run() {
		Selector selector;
		socket = new DatagramSocket(1900);
		socket->setReuseAddr();
		socket->setBroadcast();
		socket->joinGroup("239.255.255.250");

		while (!interrupted()) {
			if (selector.select(1000)) {
				char buffer[1024] = {0,};
				int len = socket->recv(buffer, sizeof(buffer));
				if (len > 0) {
					buffer[len - 1] = 0;
					cout << "[RECV] >> " << buffer << endl;
				}
			} else {
				idle(10);
			}
		}

		socket->close();
	}
};

static void thread() {
	MyThread mt;
	mt.start();
	idle(100);
}

static void ssdp() {
	SSDPServer ssdp;
	ssdp.start();
	getchar();
	ssdp.interrupt();
	ssdp.join();
}