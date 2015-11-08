#include "PollablePool.hpp"
#include <algorithm>

namespace UTIL {
    
    using namespace std;
    using namespace OS;

    /**
     * @brief Pollable
     */
    
    Pollable::Pollable() {
    }
    Pollable::~Pollable() {
    }
    
    /**
     * @brief PollingServer
     */
    
    PollablePool::PollablePool() {
    }
    PollablePool::~PollablePool() {
    }
    
    void PollablePool::registerSelector(Selector & selector) {
        for (size_t i = 0; i < pollables.size(); i++) {
            pollables[i]->registerSelector(selector);
        }
    }
    void PollablePool::unregisterSelector(Selector & selector) {
        for (size_t i = 0; i < pollables.size(); i++) {
            pollables[i]->unregisterSelector(selector);
        }
    }
    bool PollablePool::isSelected(Selector & selector) {
        for (size_t i = 0; i < pollables.size(); i++) {
            if (pollables[i]->isSelected(selector)) {
                return true;
            }
        }
        return false;
    }
    
    void PollablePool::poll(unsigned long timeout) {
        if (selector.select(timeout) > 0) {
            for (size_t i = 0; i < pollables.size(); i++) {
                if (pollables[i]->isSelected(selector)) {
                    pollables[i]->listen();
                }
            }
        }
    }
    
    void PollablePool::listen() {
        for (size_t i = 0; i < pollables.size(); i++) {
            pollables[i]->listen();
        }
    }
    
    void PollablePool::registerPollable(Pollable * pollable) {
        pollables.push_back(pollable);
        pollable->registerSelector(selector);
    }
    
    void PollablePool::unregisterPollable(Pollable * pollable) {
        pollable->unregisterSelector(selector);
        pollables.erase(std::remove(pollables.begin(), pollables.end(), pollable), pollables.end());
    }
    
    vector<Pollable*> & PollablePool::getPollables() {
        return pollables;
    }
    
    
    /**
     * @brief PollingThread
     */
    PollingThread::PollingThread(Pollable & pollable, unsigned long timeout) : pollable(pollable), timeout(timeout) {
        
    }
    PollingThread::~PollingThread() {
        
    }
    
    void PollingThread::run() {
        while (!interrupted()) {
            pollable.poll(timeout);
        }
    }
}