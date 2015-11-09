#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <liboslayer/os.hpp>
#include <liboslayer/PollablePool.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

class Server : public SelectablePollee {
private:
    ServerSocket server;
public:
    Server(SelectorPoller & poller, int port) : SelectablePollee(poller), server(port) {
    }
    virtual ~Server() {
    }
    
    void start() {
        server.setReuseAddr();
        server.bind();
        server.listen(5);
        
        registerSelecotr(server.getFd());
    }
    
    void stop() {
        
        unregisterSelector(server.getFd());
        server.close();
    }
    
    virtual void listen(SelectorPoller & poller) {
        if (poller.isSelected(server.getFd())) {
            Socket * client = server.accept();
            if (client) {
                const char * message = "hello";
                client->send(message, strlen(message));
            }
        }
    }
};


class Client : public SelectablePollee {
private:
    Socket client;
public:
    
    Client(SelectorPoller & poller, const char * host, int port) : SelectablePollee(poller), client(host, port) {
    }
    
    virtual ~Client() {
        stop();
    }
    
    void start() {
        client.connect();
        
        registerSelecotr(client.getFd());
    }
    
    void stop() {
        unregisterSelector(client.getFd());
        client.close();
    }
    
    virtual void listen(SelectorPoller & poller) {
        if (poller.isSelected(client.getFd())) {
            char buffer[1024] = {0,};
            int len = client.recv(buffer, sizeof(buffer));
            cout << string(buffer, len) << endl;
        }
    }
};


int main(int argc, char * args[]) {
    
    bool done = false;
    SelectorPoller selectorPoller;
    
    Server server(selectorPoller, 8084);
    Client client(selectorPoller, "127.0.0.1", 8084);
    
    server.start();
    client.start();
    
    cout << "start" << endl;
    
    while (!done) {
        selectorPoller.poll(1000);
    }
    
    server.stop();
    client.stop();
    
	return 0;
}