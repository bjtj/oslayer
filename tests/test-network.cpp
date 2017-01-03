#include "utils.hpp"
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

static string mac_address_to_string(const unsigned char * addr) {
	char str[30] = {0,};
	snprintf(str, sizeof(str), "%02x:%02x:%02x:%02x:%02x:%02x", addr[0], addr[1], addr[2], addr[3], addr[4], addr[5]);
	return string(str);
}

static void test_network_interface() {

	vector<NetworkInterface> ifaces = Network::getNetworkInterfaces();
	for (vector<NetworkInterface>::iterator iter = ifaces.begin(); iter != ifaces.end(); iter++) {
		unsigned char mac[6] = {0,};
		cout << iter->getName() << endl;
		iter->getMacAddress(mac, sizeof(mac));
		cout << " - MAC: " << mac_address_to_string(mac) << endl;
		cout << " - IP address(es)" << endl;
		vector<InetAddress> addrs = iter->getInetAddresses();
		for (vector<InetAddress>::iterator ai = addrs.begin(); ai != addrs.end(); ai++) {
			cout << "  * [" << ai->getHost() << "]" << endl;
		}
	}
}

int main(int argc, char *args[]) {

	test_network_interface();
    
    return 0;
}
