#include "PollablePool.hpp"
#include <algorithm>
#include "Logger.hpp"

namespace UTIL {
    
    using namespace std;
    using namespace OS;
    
    static const Logger & logger = LoggerFactory::getDefaultLogger();

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
    void Poller::onIdle() {
        for (size_t i = 0; i < pollees.size(); i++) {
            Pollee * pollee = pollees[i];
            pollee->onIdle();
        }
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
    
    LoopPoller::LoopPoller() {
        
    }
    LoopPoller::~LoopPoller() {
        
    }
    
    void LoopPoller::poll(unsigned long timeout) {
        listen();
        onIdle();
    }
    
    /**
     * @brief SelectorPoller
     */
    
    SelectorPoller::SelectorPoller() : parent(NULL) {
    }
    SelectorPoller::~SelectorPoller() {
    }
    void SelectorPoller::setParentSelectorPoller(SelectorPoller * parent) {
        this->parent = parent;
    }
    void SelectorPoller::registerSelector(int fd) {
        selector.set(fd);
        if (parent) {
            parent->registerSelector(fd);
        }
    }
    void SelectorPoller::unregisterSelector(int fd) {
        selector.unset(fd);
        if (parent) {
            parent->unregisterSelector(fd);
        }
    }
    bool SelectorPoller::isReadableSelected(int fd) {
        return (selector.isReadableSelected(fd) || (parent && parent->isReadableSelected(fd)));
    }
    bool SelectorPoller::isWriteableSelected(int fd) {
        return (selector.isWriteableSelected(fd) || (parent && parent->isWriteableSelected(fd)));
    }
    void SelectorPoller::registerSelectablePollee(SelectablePollee * pollee) {
        pollee->setSelectorPoller(this);
        registerPollee(pollee);
    }
    void SelectorPoller::unregisterSelectablePollee(SelectablePollee * pollee) {
        pollee->setSelectorPoller(NULL);
        unregisterPollee(pollee);
    }
    void SelectorPoller::registerSelectorPoller(SelectorPoller * poller) {
        poller->setParentSelectorPoller(this);
        children.push_back(poller);
    }
    void SelectorPoller::unregisterSelectorPoller(SelectorPoller * poller) {
        poller->setParentSelectorPoller(NULL);
        children.erase(std::remove(children.begin(), children.end(), poller), children.end());
        
    }
    void SelectorPoller::poll(unsigned long timeout) {
        if (selector.select(timeout) > 0) {
            listen();
        }
        onIdle();
    }
    void SelectorPoller::onIdle() {
        Poller::onIdle();
        for (size_t i = 0; i < children.size(); i++) {
            children[i]->onIdle();
        }
    }
    void SelectorPoller::listen() {
        Poller::listen();
        for (size_t i = 0; i < children.size(); i++) {
            children[i]->listen();
        }
    }
    
    
    /**
     * @brief SelectablePollee
     */
    
    SelectablePollee::SelectablePollee() : selectorPoller(NULL) {
        selfPoller.registerPollee(this);
        setSelfPoller(&selfPoller);
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
    
    PollingThread::PollingThread(Poller * poller, unsigned long timeout) : poller(poller), timeout(timeout) {
        
    }
    PollingThread::~PollingThread() {
    }
    
    void PollingThread::run() {
        while (!interrupted()) {
            poller->poll(timeout);
        }
    }
}