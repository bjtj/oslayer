#include <liboslayer/TestSuite.hpp>
#include <liboslayer/SecureSocket.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class KindVerifier : public CertificateVerifier {
public:
	KindVerifier() {}
	virtual ~KindVerifier() {}
	virtual bool onVerify(const VerifyError & err, const Certificate & cert) {
		cout << " ** onVerify() / okay : " << err.okay() << endl;
		return true;
	}
};

class TransparentVerifier : public CertificateVerifier {
public:
	TransparentVerifier() {}
	virtual ~TransparentVerifier() {}
	virtual bool onVerify(const VerifyError & err, const Certificate & cert) {
		cout << " ** onVerify() / okay : " << err.okay() << endl;
		return err.okay();
	}
};

class SecureServerThread : public Thread {
private:
	int port;
	string certPath;
	string keyPath;
public:
	SecureServerThread(int port, const string & certPath, const string & keyPath) : port(port), certPath(certPath), keyPath(keyPath) {}
	virtual ~SecureServerThread() {}
	virtual void run() {

		SecureServerSocket server(port);
		server.loadCert(certPath, keyPath);
		server.setReuseAddr(true);
		server.bind();
		server.listen(5);

		Selector selector;
		server.registerSelector(selector, Selector::READ);

		cout << "listen " << port << endl;

		while (!interrupted()) {

			if (selector.select(100)) {
				AutoRef<Socket> client(server.accept());
				if (!client.nil()) {
					cout << "connected" << endl;
					client->negotiate();
					client->send("hello", 5);
					client->close();
				}
			}
		}

		server.close();
	}
};

class SecureSocketConnectionTestCase : public TestCase {
private:
	SecureServerThread * serverThread;
	int port;
	string certPath;
	string keyPath;
public:
	SecureSocketConnectionTestCase(int port, const string & certPath, const string & keyPath) : TestCase("SecureSocketConnectionTestCase"), serverThread(NULL), port(port), certPath(certPath), keyPath(keyPath) {}
	virtual ~SecureSocketConnectionTestCase() {}
	virtual void setUp(TestEnvironment & env) {
		serverThread = new SecureServerThread(port, certPath, keyPath);
		serverThread->start();
		idle(100);
	}
	virtual void tearDown() {

		serverThread->interrupt();
		serverThread->join();
		delete serverThread;
	}
	virtual void test() {

		char buffer[1024] = {0,};

		SecureSocket client(InetAddress("localhost", port));
		KindVerifier verifier;
		client.setVerifier(&verifier);
		client.connect();
		client.recv(buffer, sizeof(buffer));
		ASSERT(string(buffer), ==, "hello");
		client.close();
	}

	int getPort() {
		return port;
	}
	string getCertPath() {
		return certPath;
	}
	string getKeyPath() {
		return keyPath;
	}
};

class SecureSocketVerifiationTestCase : public SecureSocketConnectionTestCase {
public:
	SecureSocketVerifiationTestCase(int port, const string & certPath, const string & keyPath) : SecureSocketConnectionTestCase(port, certPath, keyPath) {
	}
	virtual ~SecureSocketVerifiationTestCase() {}
	virtual void test() {
		string err;
		char buffer[1024] = {0,};
		SecureSocket client(InetAddress("localhost", getPort()));
		TransparentVerifier verifier;
		client.setVerifier(&verifier);
		try {
			client.connect();
			client.recv(buffer, sizeof(buffer));
			ASSERT(string(buffer), ==, "hello");
			
		} catch (Exception & e) {
			cerr << e.getMessage() << endl;
			err = e.getMessage();
		}

		ASSERT(err, >, "Peer");

		client.close();
	}
};

int main(int argc, char *args[]) {

	int port = 8888;
	string certPath = DATA_PATH"/cert.pem";
	string keyPath = DATA_PATH"/key.pem";

	cout << "OpenSSL Ver. " << SecureContext::getOpenSSLVersion() << endl;

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new SecureSocketConnectionTestCase(port, certPath, keyPath)));
	ts.addTestCase(AutoRef<TestCase>(new SecureSocketVerifiationTestCase(port, certPath, keyPath)));
	TestReport report(ts.testAll());
	ASSERT(report.failed(), ==, 0);
    
    return 0;
}

