#ifndef __POLLING_SERVER_HPP__
#define __POLLING_SERVER_HPP__

#include "os.hpp"
#include <vector>

namespace UTIL {
    
    class Poller;
    
    
    /**
     * @brief Pollee
     */
    class Pollee {
        
    private:
        Poller * selfPoller;
        
    public:
        
        Pollee() : selfPoller(NULL) {}
        virtual ~Pollee() {}
        
        virtual void onIdle() = 0;
        virtual void listen(Poller & poller) = 0;
        
        void setSelfPoller(Poller * selfPoller) {this->selfPoller = selfPoller;}
        Poller * getSelfPoller() {return selfPoller;}
    };
    
    
    /**
     * @brief Poller
     */
    class Poller {
        
    private:
        std::vector<Pollee*> pollees;
    public:
        
        Poller();
        virtual ~Poller();
        
        virtual void registerPollee(Pollee * pollee);
        virtual void unregisterPollee(Pollee * pollee);
        virtual void poll(unsigned long timeout) = 0;
        virtual void onIdle();
        virtual void listen();
    };
    
    /**
     * @brief LoopPoller
     */
    class LoopPoller : public Poller {
    private:
    public:
        
        LoopPoller();
        virtual ~LoopPoller();
        
        virtual void poll(unsigned long timeout);
    };
    
    class SelectablePollee;
    
    
    /**
     * @brief SelectorPoller
     */
    
    class SelectorPoller : public Poller {
        
    private:
        OS::Selector selector;
        SelectorPoller * parent;
        std::vector<SelectorPoller*> children;
        
    public:
        
        SelectorPoller();
        virtual ~SelectorPoller();
        
        void setParentSelectorPoller(SelectorPoller * parent);
        
        void registerSelector(int fd);
        void unregisterSelector(int fd);
        bool isReadableSelected(int fd);
        bool isWriteableSelected(int fd);
        
        void registerSelectablePollee(SelectablePollee * pollee);
        void unregisterSelectablePollee(SelectablePollee * pollee);
        
        void registerSelectorPoller(SelectorPoller * poller);
        void unregisterSelectorPoller(SelectorPoller * poller);
        
        virtual void poll(unsigned long timeout);
        virtual void onIdle();
        virtual void listen();
    };
    
    
    /**
     * @brief SelectablePollee
     */
    
    class SelectablePollee : public Pollee {
        
    private:
        SelectorPoller * selectorPoller;
        SelectorPoller selfPoller;
        
    public:
        
        SelectablePollee();
        virtual ~SelectablePollee();
        
        SelectorPoller * getSelectorPoller();
        void setSelectorPoller(SelectorPoller * selectorPoller);
        
        void registerSelector(int fd);
        void unregisterSelector(int fd);
        
        virtual void listen(Poller & poller);
        virtual void listen(SelectorPoller & poller) = 0;
    };
    
    
    
    
    /**
     * @brief PollingThread
     */
    class PollingThread : public OS::Thread {
    private:
        Poller * poller;
        unsigned long timeout;
        
    public:
        PollingThread(Poller * poller, unsigned long timeout);
        virtual ~PollingThread();
        
        virtual void run();
    };
}

#endif