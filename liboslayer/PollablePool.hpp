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
    
    
    
    
    class SelectorPoller;
    
    
    
    /**
     * @brief SelectablePollee
     */
    class SelectablePollee : public Pollee {
        
    private:
        SelectorPoller & selectorPoller;
        
    public:
        
        SelectablePollee(SelectorPoller & selectorPoller);
        virtual ~SelectablePollee();
        
        void registerSelecotr(int fd);
        void unregisterSelector(int fd);
        
        
        virtual void listen(Poller & poller);
        virtual void listen(SelectorPoller & poller) = 0;
    };
    
    
    
    
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
        
        virtual void poll(unsigned long timeout);
        bool isSelected(int fd);
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