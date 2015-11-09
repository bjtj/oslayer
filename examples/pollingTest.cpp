#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <liboslayer/os.hpp>
#include <liboslayer/PollablePool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class Server : public Pollable {
private:
	ServerSocket server;
public:
	Server() : server(8088) {
		server.setReuseAddr();
		server.bind();
		server.listen(5);
	}
	virtual ~Server() {
		server.close();
	}

	virtual void registerSelector(Selector & selector) {
		server.registerSelector(selector);
	}

    virtual void unregisterSelector(Selector & selector) {
		server.unregisterSelector(selector);
	}

    virtual bool isSelected(Selector & selector) {
		return server.isSelected(selector);
	}
        
    virtual void poll(unsigned long timeout) {
	}

    virtual void listen() {
		Socket * client = server.accept();
		if (client) {
			const char * message = "hello";
			client->send(message, strlen(message));
		}
	}
};


class Client : public Pollable {
private:
	Socket client;
public:
	Client(const string host, int port) : client(host.c_str(), port) {
		client.connect();
	}
	virtual ~Client() {
	}

	virtual void registerSelector(Selector & selector) {
		client.registerSelector(selector);
	}

    virtual void unregisterSelector(Selector & selector) {
		client.unregisterSelector(selector);
	}

    virtual bool isSelected(Selector & selector) {
		return client.isSelected(selector);
	}
        
    virtual void poll(unsigned long timeout) {
	}

    virtual void listen() {

		char buffer[1024] = {0,};
		int len = client.recv(buffer, sizeof(buffer));
		cout << string(buffer, len) << endl;
	}
};


class Pollee {
private:
	Selector & selector;
public:
	Pollee(Selector & selector) : selector(selector) {}
	virtual ~Pollee() {}
	void registerSelector(int fd) {
		selector.set(fd);
	}
	void unregisterSelector(int fd) {
		selector.unset(fd);
	}
	bool isSelected(int fd) {
		return selector.isSelected(fd);
	}
	virtual void listen() = 0;
};


class MyPollablePool {
private:
	Selector selector;
	vector<Pollee*> pool;
public:
	MyPollablePool() {}
	virtual ~MyPollablePool() {}
	Selector & getSelector() {
		return selector;
	}
	void addPollable(Pollee * pollable) {
		pool.push_back(pollable);
	}
	void removePollable(Pollee * pollable) {
		pool.erase(std::remove(pool.begin(), pool.end(), pollable), pool.end());
	}
	virtual void poll(unsigned long timeout) {

		if (selector.select(timeout) > 0) {
			for (size_t i = 0; i < pool.size(); i++) {
				Pollee * pollee = pool[i];
				pollee->listen();
			}
		}
	}
};


class PollableServer : public Pollee {
private:
	ServerSocket server;
public:
	PollableServer(Selector & selector, int port) : Pollee(selector), server(port) {
	}
	virtual ~PollableServer() {
	}

	virtual void start() {
		server.setReuseAddr();
		server.bind();
		server.listen(5);

		registerSelector(server.getFd());
	}

	virtual void listen() {
		if (isSelected(server.getFd())) {
			Socket * client = server.accept();
			if (client) {
				const char * message = "hello";
				client->send(message, strlen(message));
			}
		}
	}

	virtual void stop() {
		unregisterSelector(server.getFd());
		
		server.close();
	}
};

class PollableClient : public Pollee  {
private:
	Socket client;

public:


	PollableClient(Selector & selector, const string host, int port) : Pollee(selector), client(host.c_str(), port) {
	}
	virtual ~PollableClient() {
	}

	virtual void start() {
		
		client.connect();

		registerSelector(client.getFd());
		
	}

	virtual void listen() {
		if (isSelected(client.getFd())) {
			char buffer[1024] = {0,};
			int len = client.recv(buffer, sizeof(buffer));
			cout << string(buffer, len) << endl;
		}
	}

	virtual void stop() {
		unregisterSelector(client.getFd());
		client.close();
	}
};

void test() {
	PollablePool pool;

	Server server;
	Client client("127.0.0.1", 8088);

	pool.registerPollable(&server);
	pool.registerPollable(&client);

	while (1) {
		pool.poll(1000);
	}
}

void test2() {

	Selector selector;

	PollableServer server(selector, 8088);
	PollableClient client(selector, "127.0.0.1", 8088);

	server.start();
	client.start();

	while (1) {
		if (selector.select(1000) > 0) {
			server.listen();
			client.listen();
		}
	}
}

void test3() {

	MyPollablePool pool;

	PollableServer server(pool.getSelector(), 8088);
	PollableClient client(pool.getSelector(), "127.0.0.1", 8088);

	pool.addPollable(&server);
	pool.addPollable(&client);

	server.start();
	client.start();

	while (1) {
		pool.poll(1000);
	}
}

void test4() {

	

}

int main(int argc, char * args[]) {

	//test();
	//test2();
	test3();

	return 0;
}