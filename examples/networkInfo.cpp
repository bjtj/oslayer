#include <iostream>
#include <liboslayer/os.hpp>

using namespace std;
using namespace OS;

static void s_print_inet_addresses(const vector<InetAddress> & addrs) {
    for (size_t i = 0; i < addrs.size(); i++) {
        const InetAddress & addr = addrs[i];
        cout << " - " << addr.getAddress() << endl;
    }
}

static void s_test_network() {
    vector<NetworkInterface> ifaces = Network::getNetworkInterfaces();
    for (size_t i = 0; i < ifaces.size(); i++) {
        NetworkInterface & iface = ifaces[i];
        cout << iface.getName() << endl;
        
        vector<InetAddress> addrs = iface.getInetAddresses();
        s_print_inet_addresses(addrs);
//        for (size_t i = 0; i < addrs.size(); i++) {
//            InetAddress & addr = addrs[i];
//            cout << " - " << addr.getAddress() << endl;
//        }
    }
}

int main(int argc, char * args[]) {
    
    s_test_network();
    
//    s_print_inet_addresses(Network::getInetAddressesWithIfaceName("lo0"));
    
    return 0;
}