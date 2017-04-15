#include <liboslayer/os.hpp>
#include <liboslayer/Network.hpp>
#include <liboslayer/TestSuite.hpp>
#include <liboslayer/Uuid.hpp>

using namespace std;
using namespace OS;
using namespace UTIL;

/**
 * @brief 
 */
class UuidTestCase : public TestCase {
private:
	vector<string> lst;
public:
	UuidTestCase() : TestCase("uuid test") {}
	virtual ~UuidTestCase() {}
	void getMacAddress(unsigned char * out_mac) {
		vector<NetworkInterface> ifaces = Network::getNetworkInterfaces();
		for (vector<NetworkInterface>::iterator iter = ifaces.begin(); iter != ifaces.end(); iter++) {
			if (iter->isLoopBack()) {
				// 
			} else if (iter->getName() > "eth") {
				iter->getMacAddress(out_mac, sizeof(out_mac));
			}
		}
	}
	virtual void test() {
		UuidGeneratorVersion1 gen;
		unsigned char mac[6] = {0,};
		getMacAddress(mac);
		vector<uint8_t> & nodes = gen.nodes();
		nodes[0] = mac[0];
		nodes[1] = mac[1];
		nodes[2] = mac[2];
		nodes[3] = mac[3];
		nodes[4] = mac[4];
		nodes[5] = mac[5];
		for (size_t i = 0; i < 0xffff; i++) {
			testUnique(gen.generate());
		}
	}
	void testUnique(const string & uuid) {
		for (vector<string>::iterator iter = lst.begin(); iter != lst.end(); iter++) {
			if (*iter == uuid) {
				throw "Not unique";
			}
		}
		lst.push_back(uuid);
	}
};

/**
 * @brief 
 */
int main(int argc, char *args[]) {

	TestSuite ts;
	ts.addTestCase(AutoRef<TestCase>(new UuidTestCase));

	TestReport report(ts.testAll());
	report.validate();
    
    return 0;
}
