#include "os.hpp"

#define CHECK_NOT_IMPL_THROW(x) if(!x){throw NotImplementedException();}

/**
 * @namespace osl
 */
namespace osl {

    using namespace std;

    string nomeaningfulVesion() {
	return "0.1";
    }

    /**
     * @brief milliseconds sleep
     */
    void idle(unsigned long timeout) {
        
#if defined(USE_UNIX_STD)
				
	struct timespec ts;
	ts.tv_sec = timeout / 1000;
	ts.tv_nsec = (timeout % 1000) * (1000 * 1000);
	nanosleep(&ts, NULL);
        
#elif defined(USE_MS_WIN)
        
	Sleep(timeout);
        
#else
	// sleep
	throw NotImplementedException("Not implemented");
#endif
    }

    /**
     * @brief get tick count
     */
    unsigned long tick_milli() {
        
#if defined(USE_APPLE_STD)
        
        // @ref http://stackoverflow.com/a/11681069
        
        clock_serv_t cclock;
        mach_timespec_t mts;
        host_get_clock_service(mach_host_self(), SYSTEM_CLOCK, &cclock);
        clock_get_time(cclock, &mts);
        mach_port_deallocate(mach_task_self(), cclock);
        return (mts.tv_sec * 1000) + (mts.tv_nsec / 1000000);
        
#elif defined(USE_POSIX_STD)

	struct timespec spec;
	clock_gettime(CLOCK_MONOTONIC, &spec);
	return (spec.tv_sec * 1000) + (spec.tv_nsec / 1000000);
		
#elif defined(USE_MS_WIN)
        
	return GetTickCount();
        
#else
	throw NotImplementedException("Not implemented");
#endif
    }


    /**
     * @brief System
     */
    
    class SystemImpl : public System {
    public:
        SystemImpl () {
        }
        virtual ~SystemImpl() {
        }
	virtual void ignoreSigpipe() {
	}
    };
    
#if defined(USE_UNIX_STD)
	
    class NixSystemImpl : public SystemImpl {
    public:
        NixSystemImpl() {
        }
        virtual ~NixSystemImpl() {
        }
	virtual void ignoreSigpipe() {
#if defined(USE_SIGNAL)
	    signal(SIGPIPE, SIG_IGN);
#endif
	}
    };
    static NixSystemImpl s_systemImpl;
	
#elif defined(USE_MS_WIN)
	
    class MSSystemImpl : public SystemImpl {
    public:
        MSSystemImpl() {
            WSADATA wsaData;
            if (WSAStartup(MAKEWORD(2,2), &wsaData) != 0) {
		throw Exception("WSAStartup() failed", -1, 0);
	    }
        }
        virtual ~MSSystemImpl() {
            WSACleanup();
        }
    };
    static MSSystemImpl s_systemImpl;
	
#else
	
    static SystemImpl s_systemImpl;
	
#endif
    
    System * System::systemImpl = &s_systemImpl;
    
    System::System() {
    }
    System::~System() {
    }
    System * System::getInstance() {
        return systemImpl;
    }
}
