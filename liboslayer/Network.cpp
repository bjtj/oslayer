#include "Network.hpp"

namespace osl {

	using namespace std;

	/* Network Interface */
    
    NetworkInterface::NetworkInterface(const string & name) : name(name), loopback(false) {
		memset(mac_address, 0, 6);
    }
    NetworkInterface::~NetworkInterface() {
    }
    string NetworkInterface::getName() const {
        return name;
    }
	void NetworkInterface::setDescription(const string & description) {
		this->description = description;
	}
	string NetworkInterface::getDescription() const {
		return description;
	}
    void NetworkInterface::setInetAddress(const InetAddress & address) {
        inetAddresses.push_back(address);
    }
    vector<InetAddress> NetworkInterface::getInetAddresses() {
        return inetAddresses;
    }
	const vector<InetAddress> NetworkInterface::getInetAddresses() const {
        return inetAddresses;
    }
	void NetworkInterface::setMacAddress(const unsigned char * mac_address, size_t size) {
		size_t s = sizeof(this->mac_address);
		memcpy(this->mac_address, mac_address, (s < size ? s : size));
	}
	void NetworkInterface::getMacAddress(unsigned char * out, size_t size) const {
		size_t s = sizeof(this->mac_address);
		memcpy(out, this->mac_address, (s < size ? s : size));
	}
	void NetworkInterface::setLoopBack(bool loopback) {
		this->loopback = loopback;
	}
    bool NetworkInterface::isLoopBack() const {
		if (loopback) {
			return true;
		}
		for (vector<InetAddress>::const_iterator iter = inetAddresses.begin(); iter != inetAddresses.end(); iter++) {
			if (!iter->getHost().compare("127.0.0.1") ||
                !iter->getHost().compare("::1")) {
                return true;
            }
		}
        return false;
    }
    

	/* Network */
	
#if defined(USE_BSD_SOCKET)
    
    static NetworkInterface & s_obtain_network_interface(vector<NetworkInterface> & ifaces, const string & name) {
        for (size_t i = 0; i < ifaces.size(); i++) {
            if (!ifaces[i].getName().compare(name)) {
                return ifaces[i];
            }
        }
        ifaces.push_back(NetworkInterface(name));
        return s_obtain_network_interface(ifaces, name);
    }
    
    static vector<NetworkInterface> s_get_all_network_interfaces() {
        
        vector<NetworkInterface> ifaces;
        
        struct ifaddrs * addrs, * tmp;
        getifaddrs(&addrs);
        tmp = addrs;
        
        while (tmp) {
            
            if (tmp->ifa_addr) {

				NetworkInterface & iface = s_obtain_network_interface(ifaces, tmp->ifa_name);

				switch (tmp->ifa_addr->sa_family) {
				case AF_INET:
				{
					iface.setInetAddress(InetAddress((struct sockaddr*)tmp->ifa_addr));
				}
				break;
				case AF_INET6:
				{
					iface.setInetAddress(InetAddress((struct sockaddr*)tmp->ifa_addr));
				}
				break;
#if defined(USE_APPLE_STD)
				case AF_LINK:
				{
					unsigned char * ptr = (unsigned char *)LLADDR((struct sockaddr_dl *)(tmp->ifa_addr));
					iface.setMacAddress(ptr, 6);
				}
				break;
#else
				case AF_PACKET:
				{
					struct sockaddr_ll * s = (struct sockaddr_ll*)tmp->ifa_addr;
					iface.setMacAddress(s->sll_addr, 6);
				}
				break;
#endif
				default:
					break;
				}
                
            }
            
            tmp = tmp->ifa_next;
        }
        freeifaddrs(addrs);
        
        return ifaces;
    }

#elif defined(USE_WINSOCK2)

	static vector<NetworkInterface> s_get_all_network_interfaces() {

		vector<NetworkInterface> ret;

		ULONG outBufLen = 0;
		DWORD dwRetVal = 0;
		IP_ADAPTER_INFO * pAdapterInfos = (IP_ADAPTER_INFO*) malloc(sizeof(IP_ADAPTER_INFO));

		// retry up to 5 times, to get the adapter infos needed
		const int retry = 5;
		for (int i = 0; i < retry && (dwRetVal == ERROR_BUFFER_OVERFLOW || dwRetVal == NO_ERROR); ++i) {

			// GetAdaptersInfo: https://msdn.microsoft.com/ko-kr/library/windows/desktop/aa365917%28v=vs.85%29.aspx
			dwRetVal = GetAdaptersInfo(pAdapterInfos, &outBufLen);

			if (dwRetVal == NO_ERROR) {
				break;
			} else if (dwRetVal == ERROR_BUFFER_OVERFLOW) {
				free(pAdapterInfos);
				pAdapterInfos = (IP_ADAPTER_INFO *)malloc(outBufLen);
			} else {
				pAdapterInfos = NULL;
				break;
			}
		}

		if (dwRetVal == NO_ERROR) {

			IP_ADAPTER_INFO* pAdapterInfo = pAdapterInfos;

			while( pAdapterInfo ) {

				NetworkInterface iface(pAdapterInfo->AdapterName);
				iface.setDescription(pAdapterInfo->Description);

				IP_ADDR_STRING * pIpAddress = &(pAdapterInfo->IpAddressList);

				while( pIpAddress != 0 ) {

					InetAddress address;
					address.setHost(pAdapterInfo->IpAddressList.IpAddress.String);
					address.setInetVersion(InetAddress::InetVersion::INET4);
					iface.setInetAddress(address);

					pIpAddress = pIpAddress->Next;
				}
				pAdapterInfo = pAdapterInfo->Next;

				ret.push_back(iface);
			}
		}
		free(pAdapterInfos);

		return ret;
	}

#endif
    
    vector<InetAddress> Network::getInetAddressesWithIfaceName(const string & ifaceName) {
        vector<NetworkInterface> ifaces = getNetworkInterfaces();
        for (size_t i = 0; i < ifaces.size(); i++) {
            NetworkInterface & iface = ifaces[i];
            if (!iface.getName().compare(ifaceName)) {
                return iface.getInetAddresses();
            }
        }
        return vector<InetAddress>();
    }
    
    vector<NetworkInterface> Network::getNetworkInterfaces() {
        return s_get_all_network_interfaces();
    }

	vector<InetAddress> Network::getAllInetAddress() {
		vector<InetAddress> ret;
		vector<NetworkInterface> ifaces = getNetworkInterfaces();
		for (size_t i = 0; i < ifaces.size(); i++) {
			vector<InetAddress> addrs = ifaces[i].getInetAddresses();
			for (vector<InetAddress>::iterator iter = addrs.begin(); iter != addrs.end(); iter++) {
				ret.push_back(*iter);
			}
		}
		return ret;
	}
}
