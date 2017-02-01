#include "os.hpp"

#define CHECK_NOT_IMPL_THROW(x) if(!x){throw NotImplementedException();}

/**
 * @namespace OS
 */
namespace OS {

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
    
    
	/* SEMAPHORE */

#if defined(USE_APPLE_SEMAPHORE)

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		*handle = dispatch_semaphore_create(initial);
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		dispatch_semaphore_wait(*handle, DISPATCH_TIME_FOREVER);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		dispatch_semaphore_signal(*handle);
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		SUPPRESS_UNUSED(handle);
	}

#elif defined(USE_POSIX_SEMAPHORE)

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		sem_init(handle, 0, initial);
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		sem_wait(handle);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		sem_post(handle);
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		sem_destroy(handle);
	}

#else

	static void s_sem_init(SEM_HANDLE * handle, int initial) {
		*handle = CreateSemaphore(
			NULL,		// default security attributes
			initial,	// initial count
			initial,	// maximum count
			NULL);		// unnamed semaphore
	}

	static void s_sem_wait(SEM_HANDLE * handle) {
		WaitForSingleObject(*handle, INFINITE);
	}

	static void s_sem_post(SEM_HANDLE * handle) {
		ReleaseSemaphore( 
                        *handle,	// handle to semaphore
                        1,			// increase count by one
                        NULL);		// not interested in previous count
	}

	static void s_sem_destroy(SEM_HANDLE * handle) {
		CloseHandle(*handle);
	}

#endif /* SEMAPHORE */
	

	Semaphore::Semaphore(int initial) : initial(initial) {
		s_sem_init(&handle, initial);
	}

	Semaphore::Semaphore(const Semaphore & other) {
		this->initial = other.initial;
		s_sem_init(&(this->handle), this->initial);
	}
	
	Semaphore::~Semaphore() {
		s_sem_destroy(&handle);
	}
	
	void Semaphore::wait() const {
		s_sem_wait(&handle);
	}
	
	void Semaphore::post() const {
        s_sem_post(&handle);
	}

    
    /**
     * @breif auto lock
     */
    
    AutoLock::AutoLock(Semaphore & sem) : sem(sem) {
        sem.wait();
    }
    AutoLock::~AutoLock() {
        sem.post();
    }
    

	/* THREAD */
#if defined(USE_PTHREAD)

	// unix or linux

	typedef void * (*thread_func)(void *);
	
	static void * s_thread_wrapper(void * arg) {

		
		Thread * thread = (Thread*)arg;

#if defined(USE_PRCTL)
		{
			char name[16] = {0,};
			snprintf(name, sizeof(name), "tid:0x%x", (unsigned int)thread->getId());
			prctl(PR_SET_NAME, name, 0, 0, 0);
		}
#endif

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief pthread_create
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
	
		pthread_attr_t attr;
		bool started = false;

		if (!handle || !thread) {
			return false;
		}
		if (pthread_attr_init(&attr) != 0) {
			return false;
		}

		try {
			if (pthread_attr_setdetachstate(&attr, PTHREAD_CREATE_DETACHED) != 0) {
				throw Exception("pthread_attr_setdetachstate() failed");
			}
			if (stack_size > 0) {
				if (pthread_attr_setstacksize(&attr, stack_size) != 0) {
					throw Exception("pthread_attr_setstacksize() failed");
				}
			}
			if (pthread_create(handle, &attr, func, (void*)thread) != 0) {
				throw Exception("pthread_create() failed");
			}
			started = true;
			
		} catch (Exception e) {
			// ...
		}

		pthread_attr_destroy(&attr);
		return started;
	}

#elif defined(USE_WIN_THREAD)

	// windows

	typedef UINT (WINAPI thread_func)(void *);

	static UINT WINAPI s_thread_wrapper(void * arg) {

		Thread * thread = (Thread*)arg;

		thread->run();

		thread->reset();

		return 0;
	}

	/**
	 * @brief win32 thread start
	 */
	static bool s_startThread(THREAD_HANDLE * handle, thread_func func, Thread * thread, size_t stack_size) {
		UINT dwThreadID;
		HANDLE h;

		if (!handle || !thread) {
			return false;
		}

		h = (HANDLE)_beginthreadex(NULL, stack_size, func, (void*)thread, 0, &dwThreadID);
		*handle = h;
		
		return false;
	}

#endif

	/**
	 * @brief thread id seed - 0 means error
	 */
	unsigned int Thread::s_thread_id_seed = 0;

	/**
	 * @brief thread constructor
	 */
	Thread::Thread() : handle(0), signal_interrupt(false), stack_size(0) {
		init();
	}

	Thread::Thread(size_t stack_size) : handle(0), signal_interrupt(false), stack_size(stack_size) {
		init();
	}
	
	Thread::~Thread() {
	}

	void Thread::init() {
		id = (++s_thread_id_seed == 0) ? ++s_thread_id_seed : s_thread_id_seed;

		reset();
	}

	unsigned int Thread::getId() {
		return id;
	}

	void Thread::reset() {
		signal_interrupt = false;
		running = false;
	}

	bool Thread::start() {
		
		if (!isRunning()) {
			bool ret = s_startThread(&handle, s_thread_wrapper, this, stack_size);
			running = true;
			return ret;
		}
		return false;
	}
	
	void Thread::interrupt() {
		signal_interrupt = true;
		onInterrupt();
	}

	bool Thread::interrupted() {
		bool ret = signal_interrupt;
		signal_interrupt = false;
		return ret;
	}

	bool Thread::isRunning() {
		return running;
	}

	void Thread::onInterrupt() {
		/* virtual */
	}

	void Thread::wait() {
		while (running) { idle(10); }
	}

    /* SELECTION */
    
    Selection::Selection(int fd, bool readable, bool writeable, bool except)
		: fd(fd), readable(readable), writeable(writeable), except(except) {
        
    }
    Selection::~Selection() {
        
    }
    
    int Selection::getFd() {
        return fd;
    }
    
    bool Selection::isReadable() {
        return readable;
    }
    
    bool Selection::isWritable() {
        return writeable;
    }

	bool Selection::isExcept() {
		return except;
	}

	/* Selectable */

	Selectable::Selectable() {
	}
	
	Selectable::~Selectable() {
	}
    
    // bool Selectable::isSelectable() {
    //     return selectable;
    // }
    // void Selectable::setSelectable(bool selectable) {
    //     this->selectable = selectable;
    // }
	void Selectable::registerSelector(Selector & selector, unsigned char flags) {
		selector.set(getFd(), flags);
	}
	
	void Selectable::unregisterSelector(Selector & selector, unsigned char flags) {
		selector.unset(getFd(), flags);
	}
	
	bool Selectable::isReadable(Selector & selector) {
		return selector.isReadable(getFd());
	}

	bool Selectable::isWritable(Selector & selector) {
		return selector.isWritable(getFd());
	}
	
	bool Selectable::isExcept(Selector & selector) {
		return selector.isExcept(getFd());
	}

	/* Selector */

	Selector::Selector() : maxfds(0) {
		FD_ZERO(&readfds);
        FD_ZERO(&writefds);
		FD_ZERO(&exceptfds);
		FD_ZERO(&curreadfds);
        FD_ZERO(&curwritefds);
		FD_ZERO(&curexceptfds);
	}
	
	Selector::~Selector() {
	}
	void Selector::set(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("invalid fd", fd, 0);
		}
		
		if (fd > maxfds) {
			maxfds = fd;
		}

		if (flags & Selector::READ) {
			FD_SET(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_SET(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_SET(fd, &exceptfds);
		}
	}
	void Selector::unset(int fd, unsigned char flags) {

		if (fd < 0) {
			throw IOException("Invalid fd", fd, 0);
		}

		if (flags & Selector::READ) {
			FD_CLR(fd, &readfds);
		}
		if (flags & Selector::WRITE) {
			FD_CLR(fd, &writefds);
		}
		if (flags & Selector::EXCEPT) {
			FD_CLR(fd, &exceptfds);
		}
	}
	int Selector::select(unsigned long timeout_milli) {

		struct timeval timeout;
		timeout.tv_sec = timeout_milli / 1000;
		timeout.tv_usec = (timeout_milli % 1000) * 1000;
		
		curreadfds = readfds;
        curwritefds = writefds;
		curexceptfds = exceptfds;
		
		return ::select(maxfds + 1, &curreadfds, &curwritefds, &curexceptfds, &timeout);
	}
	vector<Selection> & Selector::getSelections() {
		selections.clear();
		for (int i = 0; i < maxfds + 1; i++) {
            
            bool readable = FD_ISSET(i, &curreadfds) ? true : false;
            bool writeable = FD_ISSET(i, &curwritefds) ? true : false;
			bool error = FD_ISSET(i, &curexceptfds) ? true : false;
            
            if (readable || writeable || error) {
                selections.push_back(Selection(i, readable, writeable, error));
            }
		}
		return selections;
	}
    
    bool Selector::isRegistered(int fd, unsigned char type) {
        switch (type) {
        case READ:
            return FD_ISSET(fd, &readfds) ? true : false;
        case WRITE:
            return FD_ISSET(fd, &writefds) ? true : false;
        case EXCEPT:
            return FD_ISSET(fd, &exceptfds) ? true : false;
        default:
            return false;
        }
    }
    
    bool Selector::isReadable(int fd) {
        return FD_ISSET(fd, &curreadfds) ? true : false;
    }
    
    bool Selector::isReadable(Selectable & selectable) {
        return isReadable(selectable.getFd());
    }
    
    bool Selector::isWritable(int fd) {
        return FD_ISSET(fd, &curwritefds) ? true : false;
    }
    
    bool Selector::isWritable(Selectable & selectable) {
        return isWritable(selectable.getFd());
    }

	bool Selector::isExcept(int fd) {
        return FD_ISSET(fd, &curexceptfds) ? true : false;
    }
    
    bool Selector::isExcept(Selectable & selectable) {
        return isExcept(selectable.getFd());
    }



	SharedSelector::SharedSelector() : semSet(1), semCur(1) {
	}
	SharedSelector::~SharedSelector() {
	}
	void SharedSelector::set(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::set(fd, flags);
	}
	void SharedSelector::unset(int fd, unsigned char flags) {
		AutoLock lock(semSet);
		Selector::unset(fd, flags);
	}
	int SharedSelector::select(unsigned long timeout_milli) {
		AutoLock lock(semCur);
		return Selector::select(timeout_milli);
	}
	
	std::vector<Selection> & SharedSelector::getSelections() {
		AutoLock lock(semCur);
		return Selector::getSelections();
	}

	bool SharedSelector::isReadable(int fd) {
		AutoLock lock(semCur);
		return Selector::isReadable(fd);
	}
	bool SharedSelector::isReadable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isReadable(selectable);
	}
	bool SharedSelector::isWritable(int fd) {
		AutoLock lock(semCur);
		return Selector::isWritable(fd);
	}
	bool SharedSelector::isWritable(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isWritable(selectable);
	}
	bool SharedSelector::isExcept(int fd) {
		AutoLock lock(semCur);
		return Selector::isExcept(fd);
	}
	bool SharedSelector::isExcept(Selectable & selectable) {
		AutoLock lock(semCur);
		return Selector::isExcept(selectable);
	}
}
