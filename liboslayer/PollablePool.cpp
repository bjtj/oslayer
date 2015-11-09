#include "PollablePool.hpp"
#include <algorithm>

namespace UTIL {
    
    using namespace std;
    using namespace OS;

    /**
     * @brief Poller
     */
    Poller::Poller() {
    }
    Poller::~Poller() {
    }
    void Poller::registerPollee(Pollee * pollee) {
        pollees.push_back(pollee);
    }
    void Poller::unregisterPollee(Pollee * pollee) {
        pollees.erase(std::remove(pollees.begin(), pollees.end(), pollee), pollees.end());
    }
    void Poller::listen() {
        for (size_t i = 0; i < pollees.size(); i++) {
            Pollee * pollee = pollees[i];
            pollee->listen(*this);
        }
    }
    
    
    /**
     * @brief SelectablePollee
     */
    
    SelectablePollee::SelectablePollee(SelectorPoller & selectorPoller) : selectorPoller(selectorPoller) {
        selectorPoller.registerPollee(this);
    }
    SelectablePollee::~SelectablePollee() {
    }
    void SelectablePollee::registerSelecotr(int fd) {
        selectorPoller.registerSelector(fd);
    }
    void SelectablePollee::unregisterSelector(int fd) {
        selectorPoller.unregisterSelector(fd);
    }
    void SelectablePollee::listen(Poller & poller) {
        listen((SelectorPoller&)poller);
    }
    
    
    
    /**
     * @brief SelectorPoller
     */
    
    SelectorPoller::SelectorPoller() {
    }
    SelectorPoller::~SelectorPoller() {
    }
    void SelectorPoller::registerSelector(int fd) {
        selector.set(fd);
    }
    void SelectorPoller::unregisterSelector(int fd) {
        selector.unset(fd);
    }
    void SelectorPoller::poll(unsigned long timeout) {
        if (selector.select(timeout) > 0) {
            listen();
        }
    }
    
    bool SelectorPoller::isSelected(int fd) {
        return selector.isSelected(fd);
    }
    
    
    /**
     * @brief PollingThread
     */
    
    PollingThread::PollingThread(Poller & poller, unsigned long timeout) : poller(poller), timeout(timeout) {
        
    }
    PollingThread::~PollingThread() {
    }
    
    void PollingThread::run() {
        while (!interrupted()) {
            poller.poll(timeout);
        }
    }
}