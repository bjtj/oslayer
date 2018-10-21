#ifndef __MUTEX_HPP__
#define __MUTEX_HPP__

#include "os.hpp"

namespace osl {

    class Mutex
    {
    private:
#if defined(USE_PTHREAD)		// apple also
	pthread_mutex_t _mutex;
	pthread_cond_t _cond;
#elif defined(USE_MS_WIN)
	HANDLE _mutex;
	HANDLE _evt;
#else
	// not support
#endif
    public:
	Mutex();
	virtual ~Mutex();
	void lock();
	void unlock();
    };
}

#endif
