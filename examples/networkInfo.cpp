#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

static void s_print_inet_addresses(const vector<InetAddress> & addrs) {
    for (size_t i = 0; i < addrs.size(); i++) {
        const InetAddress & addr = addrs[i];
        cout << " - " << addr.getHost() << endl;
    }
}

static void s_test_network() {
    vector<NetworkInterface> ifaces = Network::getNetworkInterfaces();
    for (size_t i = 0; i < ifaces.size(); i++) {
        NetworkInterface & iface = ifaces[i];
        cout << iface.getName() << endl;
		cout << " :: " << iface.getDescription() << endl;
        
        vector<InetAddress> addrs = iface.getInetAddresses();
        s_print_inet_addresses(addrs);
    }
}

int main(int argc, char * args[]) {
    
    s_test_network();

	getchar();
    
    return 0;
}
