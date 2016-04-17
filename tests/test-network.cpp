#include "utils.hpp"
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

static void test_network_interface() {

	vector<NetworkInterface> ifaces = Network::getNetworkInterfaces();
	for (vector<NetworkInterface>::iterator iter = ifaces.begin(); iter != ifaces.end(); iter++) {
		cout << iter->getName() << endl;
		vector<InetAddress> addrs = iter->getInetAddresses();
		for (vector<InetAddress>::iterator ai = addrs.begin(); ai != addrs.end(); ai++) {
			cout << " - " << ai->getHost() << endl;
		}
	}
}

int main(int argc, char *args[]) {

	test_network_interface();
    
    return 0;
}
