#include "utils.hpp"
#include <liboslayer/os.hpp>
#include <liboslayer/Socket.hpp>
#include <liboslayer/AutoRef.hpp>
#include <liboslayer/Text.hpp>
#include <liboslayer/Timer.hpp>

#define LOG cout << "[" << _t() << "] "

using namespace std;
using namespace osl;


static string toString(InetAddress remoteAddr);
static unsigned long _t();
static string readonly(InetAddress remoteAddr);
static string readonly(InetAddress remoteAddr, unsigned long connectionTimeout, unsigned long recvTimeout);

class ClientHandler {
private:
public:
    ClientHandler() {}
    virtual ~ClientHandler() {}
    virtual void handle(AutoRef<Socket> client) = 0;
};

class SimpleHandler : public ClientHandler {
private:
public:
    SimpleHandler() {}
    virtual ~SimpleHandler() {}
    virtual void handle(AutoRef<Socket> client) {
	string msg = "hello";
	client->send(msg.c_str(), msg.size());
    }
};

class TimeoutHandler : public ClientHandler {
private:
    unsigned long timeout;
public:
    TimeoutHandler(unsigned long timeout) : timeout(timeout) {}
    virtual ~TimeoutHandler() {}
    virtual void handle(AutoRef<Socket> client) {
	unsigned long tick = tick_milli();
	idle(timeout);
	string msg = "duration - " + Text::toString(tick_milli() - tick);
	client->send(msg.c_str(), msg.size());
    }
};

class SlowHandler : public ClientHandler {
private:
    unsigned long timeout;
public:
    SlowHandler(unsigned long timeout) : timeout(timeout) {}
    virtual ~SlowHandler() {}
    virtual void handle(AutoRef<Socket> client) {
		
	string msg = "partial";
	client->send(msg.c_str(), msg.size());
		
	unsigned long tick = tick_milli();
	idle(timeout);
		
	msg = "duration - " + Text::toString(tick_milli() - tick);
	client->send(msg.c_str(), msg.size());
    }
};

class ServerThread : public Thread {
private:
    int port;
    AutoRef<ClientHandler> handler;
public:
    ServerThread(int port, AutoRef<ClientHandler> handler) : port(port), handler(handler) {}
    virtual ~ServerThread() {}
    virtual void run() {
	ServerSocket server(port);

	server.setReuseAddr(true);
	server.bind();
	server.listen(5);

	Selector selector;
	server.registerSelector(selector, Selector::READ);

	InetAddress serverAddr = server.getLocalInetAddress();
	printf(" ** [S] listening... %s:%d\n", serverAddr.getHost().c_str(), serverAddr.getPort());

	while (!interrupted()) {
	    if (selector.select(1000) > 0) {
		AutoRef<Socket> client(server.accept());
		if (!client.nil()) {
		    InetAddress localAddr = client->getLocalInetAddress();
		    InetAddress remoteAddr = client->getRemoteInetAddress();
		    printf("[%lu] ** [S] connected from %s:%d [local - %s:%d]\n", _t(), remoteAddr.getHost().c_str(), remoteAddr.getPort(), localAddr.getHost().c_str(), localAddr.getPort());

		    LOG << " ** [S] start handling client / " << toString(remoteAddr) << endl;
		    handler->handle(client);
		    LOG << " ** [S] start handling client - done / " << toString(remoteAddr) << endl;
		    client->close();
		}
	    }			
	}
	server.close();
    }
};

static string toString(InetAddress remoteAddr) {
    return remoteAddr.getHost() + ":" + Text::toString(remoteAddr.getPort());
}

static unsigned long _t() {
    static unsigned long base = tick_milli();
    return tick_milli() - base;
}

static string readonly(InetAddress remoteAddr) {
    return readonly(remoteAddr, 0, 0);
}

static string readonly(InetAddress remoteAddr, unsigned long connectionTimeout, unsigned long recvTimeout) {

    string ret;
	
    Socket sock(remoteAddr);

    LOG << " ** connect" << endl;
    if (connectionTimeout > 0) {
	LOG << " ** set connection timeout : " << connectionTimeout << endl;
	sock.connect(connectionTimeout);
    } else {
	sock.connect();
    }
    LOG << " ** connect - done" << endl;

    if (recvTimeout > 0) {
	LOG << " ** set receive timeout : " << recvTimeout << endl;
	LOG << "Before: " << sock.getRecvTimeout() << endl;
	sock.setRecvTimeout(recvTimeout);
	LOG << "After: " << sock.getRecvTimeout() << endl;
    }
	

    char buffer[1024] = {0,};
    size_t len = 0;
    try {
	while ((len = sock.recv(buffer, sizeof(buffer))) > 0) {
	    LOG << " ** receive" << endl;
	    ret.append(buffer, len);
	    LOG << " ** receive - done" << endl;
	}
    } catch (IOException e) {
	if (e.error_code() != 0) {
	    throw e;
	}
    }
	
    sock.close();
	
    return ret;
}


static void test_socket_timeout() {

    LOG << " ** @@@ receive test" << endl;
    {
	string ret = readonly(InetAddress("127.0.0.1", 9999));
	LOG << " ** result : " << ret << endl;
	ASSERT(ret, ==, "hello");
    }
	
    LOG << " ** @@@ timeout test" << endl;
    string err;
    try {
	string ret = readonly(InetAddress("127.0.0.1", 9998), 0, 1000);
	LOG << ret << " !!! unexpected" << endl;
    } catch (Exception & e) {
	LOG << e.what() << endl;
	err = e.what();
    }
    ASSERT(err, >, "recv() error");

    LOG << " ** @@@ slow test" << endl;
    err = "";
    try {
	string ret = readonly(InetAddress("127.0.0.1", 9997), 0, 1000);
	LOG << ret << " !!! unexpected" << endl;
    } catch (Exception & e) {
	LOG << e.what() << endl;
	err = e.what();
    }
    ASSERT(err, >, "recv() error");
}

static string readonly_multiplex(InetAddress remoteAddr, unsigned long recvTimeout) {

    LOG << " ** @@@ multiplex test" << endl;

    Timeout readTimeout;

    string ret;
    Socket sock(remoteAddr);

    sock.connect();

    if (recvTimeout > 0) {
	LOG << " ** set timeout" << endl;
	readTimeout.reset();
	readTimeout.value() = recvTimeout;
	// sock.setRecvTimeout(recvTimeout);
    }
	
    Selector selector;
    sock.registerSelector(selector, Selector::READ | Selector::EXCEPT);

    LOG << " ** [C] listening..." << endl;

    while (1) {
	if (selector.select(1000) > 0) {
	    bool w = sock.isWritable(selector);
	    bool r = sock.isReadable(selector);
	    bool e = sock.isExcept(selector);
	    LOG << "wre : " << w << r << e << endl;

	    if (r) {
		readTimeout.reset();
		char buffer[1024] = {0,};
		try {
		    sock.recv(buffer, sizeof(buffer));
		} catch (IOException e) {
		    ASSERT(e.error_code(), ==, 0);
		    break;
		}
		LOG << " >> " << buffer << endl;
		ret.append(buffer);
	    }

	    if (e) {
		throw Exception("error");
	    }
	}

	if (readTimeout.value() > 0 && readTimeout.expired()) {
	    throw Exception("recv timeout");
	}
    }


    sock.close();
    return ret;
}

static void test_multiplex() {
	
    ASSERT(readonly_multiplex(InetAddress("127.0.0.1", 9999), 0), ==, "hello");
	
    string err;
    try {
	readonly_multiplex(InetAddress("127.0.0.1", 9998), 1000);
    } catch (Exception & e) {
	err = e.what();
	LOG << err << endl;
    }
    ASSERT(err, ==, "recv timeout");
}

int main(int argc, char *args[]) {

    cout << "server thread" << endl;
    ServerThread st(9999, AutoRef<ClientHandler>(new SimpleHandler));
    st.start();

    cout << "server thread2" << endl;
    ServerThread st2(9998, AutoRef<ClientHandler>(new TimeoutHandler(2000)));
    st2.start();

    cout << "server thread3" << endl;
    ServerThread st3(9997, AutoRef<ClientHandler>(new SlowHandler(2000)));
    st3.start();

    cout << "[idle 100]" << endl;
    idle(100);

    cout << "[test socket timeout]" << endl;
    test_socket_timeout();
	
    cout << "[test multiplex]" << endl;
    test_multiplex();

    cout << "[server thread / interrupt & wait]" << endl;
    st.interrupt();
    st.join();

    cout << "[server thread2 / interrupt & wait]" << endl;
    st2.interrupt();
    st2.join();
	
    cout << "[server thread3 / interrupt & wait]" << endl;
    st3.interrupt();
    st3.join();
    
    return 0;
}

