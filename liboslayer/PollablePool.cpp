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
    void SelectorPoller::registerSelectablePollee(SelectablePollee * pollee) {
        pollee->setSelectorPoller(this);
        registerPollee(pollee);
    }
    void SelectorPoller::unregisterSelectablePollee(SelectablePollee * pollee) {
        pollee->setSelectorPoller(NULL);
        unregisterPollee(pollee);
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
     * @brief SelectablePollee
     */
    
    SelectablePollee::SelectablePollee() : selectorPoller(NULL) {
        selfPoller.registerPollee(this);
    }
    SelectablePollee::~SelectablePollee() {
        selfPoller.unregisterPollee(this);
    }
    SelectorPoller * SelectablePollee::getSelectorPoller() {
        return selectorPoller;
    }
    void SelectablePollee::setSelectorPoller(SelectorPoller * selectorPoller) {
        this->selectorPoller = selectorPoller;
    }
    SelectorPoller & SelectablePollee::getSelfSelectorPoller() {
        return selfPoller;
    }
    void SelectablePollee::registerSelector(int fd) {
        selfPoller.registerSelector(fd);
        if (selectorPoller) {
            selectorPoller->registerSelector(fd);
        }
    }
    void SelectablePollee::unregisterSelector(int fd) {
        selfPoller.unregisterSelector(fd);
        if (selectorPoller) {
            selectorPoller->unregisterSelector(fd);
        }
    }
    void SelectablePollee::listen(Poller & poller) {
        listen((SelectorPoller&)poller);
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