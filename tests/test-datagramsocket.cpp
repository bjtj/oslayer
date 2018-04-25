#include <liboslayer/os.hpp>
#include <liboslayer/Thread.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/DatagramSocket.hpp>

using namespace osl;


class DatagramServer : public Thread {
private:
	int port;
public:
    DatagramServer(int port) : port(port) {
	}
    virtual ~DatagramServer() {
	}
	virtual void run() {
		printf("Server started\n");
		DatagramSocket socket(port);
		DatagramPacket packet(1024);
		socket.recv(packet);
		printf("length: %ld\n", packet.getLength());
		printf("data: %s\n", packet.getData());
		socket.close();
	}
};


class DatagramSocketTestCase : public TestCase {
private:
	DatagramServer * server;
public:
    DatagramSocketTestCase() : TestCase("datagram socket test"), server(NULL) {
		
	}
    virtual ~DatagramSocketTestCase() {
	}
	virtual void setUp(TestEnvironment & env) {
		server = new DatagramServer(8080);
		server->start();
		idle(1000);
	}
	virtual void tearDown() {
		server->interrupt();
		server->wait();
		delete server;
	}
	virtual void test() {
		DatagramSocket socket;
		DatagramPacket packet(1024, "127.0.0.1", 8080);
		packet.write("hello");
		packet.write(" world");
		socket.send(packet);
		socket.close();
	}
};


int main(int argc, char *args[]) {

	TestSuite suite;
	suite.addTestCase(AutoRef<TestCase>(new DatagramSocketTestCase));

	TestReport report(suite.testAll());
	report.validate();
    
    return 0;
}

