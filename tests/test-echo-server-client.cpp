#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/Thread.hpp>
#include <string>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * echo server
 */
class EchoServer : public Thread{
private:
	int _port;
public:
    EchoServer(int port) : _port(port) {
	}
    virtual ~EchoServer() {
	}
	virtual void run() {
		ServerSocket server(_port);
		server.setReuseAddr(true);
		server.bind();
		server.listen(5);
		while (!interrupted()) {
			Socket * client = server.accept();
			char buffer[1024] = {0,};
			client->send(buffer, client->recv(buffer, sizeof(buffer)));
			client->close();
			if (string(buffer) == "quit") {
				break;
			}
		}
		server.close();
	}
};

/**
 * 
 */
class EchoServerClientTestCase : public TestCase {
public:
    EchoServerClientTestCase() : TestCase("echo server client test") {
	}
    virtual ~EchoServerClientTestCase() {
	}
	virtual void test() {
		EchoServer server(9000);
		server.start();

		idle(100);

		for (int i = 0; i < 100; i++) {
			Socket client(InetAddress("127.0.0.1", 9000));
			client.connect();
			client.send("hello", 5);
			char buffer[1024] = {0,};
			client.recv(buffer, sizeof(buffer));
			ASSERT(string(buffer), ==, "hello");
			client.close();
		}

		Socket client(InetAddress("127.0.0.1", 9000));
		client.connect();
		client.send("quit", 5);
		char buffer[1024] = {0,};
		client.recv(buffer, sizeof(buffer));
		ASSERT(string(buffer), ==, "quit");
		client.close();

		server.join();
	}
};

int main(int argc, char *argv[])
{
	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new EchoServerClientTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
