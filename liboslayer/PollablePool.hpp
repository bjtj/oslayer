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
    public:
        
        Pollee() {}
        virtual ~Pollee() {}
        
        virtual void listen(Poller & poller) = 0;
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
        virtual void listen();
    };
    
    
    class SelectablePollee;
    
    
    /**
     * @brief SelectorPoller
     */
    
    class SelectorPoller : public Poller {
        
    private:
        OS::Selector selector;
        
    public:
        
        SelectorPoller();
        virtual ~SelectorPoller();
        
        void registerSelector(int fd);
        void unregisterSelector(int fd);
        
        void registerSelectablePollee(SelectablePollee * pollee);
        void unregisterSelectablePollee(SelectablePollee * pollee);
        
        virtual void poll(unsigned long timeout);
        bool isSelected(int fd);
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
        
        SelectorPoller & getSelfSelectorPoller();
        
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
        Poller & poller;
        unsigned long timeout;
        
    public:
        PollingThread(Poller & poller, unsigned long timeout);
        virtual ~PollingThread();
        
        virtual void run();
    };
}

#endif