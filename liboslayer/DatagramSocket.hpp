#ifndef __DATAGRAM_SOCKET_HPP__
#define __DATAGRAM_SOCKET_HPP__

#include "os.hpp"
#include "SocketApi.hpp"
#include "Selector.hpp"

namespace osl {

    /**
     * @brief DatagramSocket
     */
    class DatagramSocket : public SocketOptions, public Selectable {
    private:
	DatagramSocket * impl;

    public:
	DatagramSocket();
	DatagramSocket(int port);
	DatagramSocket(const InetAddress & bindAddr);
	virtual ~DatagramSocket();
    protected:
	bool created();
	void setImpl(DatagramSocket * impl);
	virtual void createImpl();
	virtual void createImpl(int port);
	virtual void createImpl(const InetAddress & bindAddr);
	virtual DatagramSocket & getImpl();
    public:
	virtual SOCK_HANDLE getSocket();
	virtual int getFd();
	virtual void bind(const InetAddress & addr);
	virtual void connect(const InetAddress & bindAddr);
	virtual void disconnect();
	virtual void close();
	virtual bool isClosed();
	virtual int recv(DatagramPacket & packet);
	virtual int send(DatagramPacket & packet);
	virtual InetAddress getLocalInetAddress();
	virtual void setBroadcast(bool broadcast);

	
    };


    /**
     * @brief MulticastSocket
     */
    class MulticastSocket : public DatagramSocket {
    private:
    public:
	MulticastSocket();
	MulticastSocket(int port);
	MulticastSocket(const InetAddress & addr);
	virtual ~MulticastSocket();

	virtual void joinGroup(const std::string & group);
	virtual void setTimeToLive(int ttl);

    protected:
	virtual void createImpl();
	virtual void createImpl(int port);
	virtual void createImpl(const InetAddress & bindAddr);
    };
}

#endif
