#ifndef __POLLING_SERVER_HPP__
#define __POLLING_SERVER_HPP__

#include "os.hpp"
#include <vector>

namespace UTIL {
    
    /**
     * @brief pollable
     */
    class Pollable : OS::Selectable {
    private:
    public:
        Pollable();
        virtual ~Pollable();
        
        virtual void registerSelector(OS::Selector & selector) = 0;
        virtual void unregisterSelector(OS::Selector & selector) = 0;
        virtual bool isSelected(OS::Selector & selector) = 0;
        
        virtual void poll(unsigned long timeout) = 0;
        virtual void listen() = 0;
    };
    
    /**
     * @brief polling server
     */
    class PollablePool : public Pollable {
    private:
        OS::Selector selector;
        std::vector<Pollable*> pollables;
        
    public:
        PollablePool();
        virtual ~PollablePool();
        
        virtual void registerSelector(OS::Selector & selector);
        virtual void unregisterSelector(OS::Selector & selector);
        virtual bool isSelected(OS::Selector & selector);
        
        virtual void poll(unsigned long timeout);
        virtual void listen();
        
        void registerPollable(Pollable * pollable);
        void unregisterPollable(Pollable * pollable);
        
    protected:
        std::vector<Pollable*> & getPollables();
    };
    
    /**
     * @brief polling thread
     */
    class PollingThread : public OS::Thread {
    private:
        Pollable & pollable;
        unsigned long timeout;
        
    public:
        PollingThread(Pollable & pollable, unsigned long timeout);
        virtual ~PollingThread();
        
        virtual void run();
    };
}

#endif