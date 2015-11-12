#include "RandomPortBinders.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
     * @brief RangeRandomPortBinder
     */

	RangeRandomPortBinder::RangeRandomPortBinder(int startPort, int endPort)
	: startPort(startPort), currentPort(0), endPort(endPort), selectedPort(0) {
	}
    RangeRandomPortBinder::~RangeRandomPortBinder() {
	}
    void RangeRandomPortBinder::start() {
		currentPort = startPort;
		selectedPort = 0;
	}
    int RangeRandomPortBinder::getNextPort() {
		selectedPort = currentPort++;
		return selectedPort;
	}
    bool RangeRandomPortBinder::wantFinish() {
		return currentPort > endPort;
	}
    int RangeRandomPortBinder::getSelectedPort() {
		return selectedPort;
	}

	/**
     * @brief ListRandomPortBinder
     */

	ListRandomPortBinder::ListRandomPortBinder(const vector<int> & portLists) : currentIndex(0), selectedPort(0) {
	}
    ListRandomPortBinder::~ListRandomPortBinder() {
	}
    void ListRandomPortBinder::start() {
		currentIndex = 0;
		selectedPort = 0;
	}
    int ListRandomPortBinder::getNextPort() {
		selectedPort = portLists[currentIndex++];
		return selectedPort;
	}
    bool ListRandomPortBinder::wantFinish() {
		return currentIndex >= portLists.size();
	}
    int ListRandomPortBinder::getSelectedPort() {
		return selectedPort;
	}
}
