#ifndef __NETWORK_HPP__
#define __NETWORK_HPP__

#include "os.hpp"
#include "SocketApi.hpp"

namespace OS {

	/**
     * @brief network interface
     */
    class NetworkInterface {
	private:
		std::string name;
		std::string description;
		std::vector<InetAddress> inetAddresses;
		char mac_address[6];
		bool loopback;
	public:
		NetworkInterface(const std::string & name);
		virtual ~NetworkInterface();
		std::string getName() const;
		void setDescription(const std::string & description);
		std::string getDescription() const;
		void setInetAddress(const InetAddress & address);
		std::vector<InetAddress> getInetAddresses();
		const std::vector<InetAddress> getInetAddresses() const;
		void setMacAddress(const unsigned char * mac_address, size_t size);
		void getMacAddress(unsigned char * out, size_t size) const;
		void setLoopBack(bool loopback);
		bool isLoopBack() const;
    };

	/**
	 * @brief network
	 */
	class Network {
	private:
	public:
        static std::vector<InetAddress> getInetAddressesWithIfaceName(const std::string & ifaceName);
        static std::vector<NetworkInterface> getNetworkInterfaces();
		static std::vector<InetAddress> getAllInetAddress();
	};
}

#endif
