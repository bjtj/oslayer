#ifndef __EVENT_HPP__
#define __EVENT_HPP__

#include "os.hpp"

namespace osl {

    /**
     * timeout exception
     */
    DECL_EXCEPTION(TimeoutException, IOException);

    /**
     * event
     */
    class Event
    {
    private:
#if defined(USE_PTHREAD)
	pthread_mutex_t _mutex;
	pthread_cond_t _cond;
#elif defined(USE_MS_WIN)
	HANDLE _mutex;
	HANDLE _evt;
#endif
    private:
	Event(const Event & other); // not allowed
	Event & operator=(const Event & other); // not allowed
    public:
	Event();
	virtual ~Event();
	void lock();
	void unlock();
	void wait();
	void wait(unsigned long timeout);
	void notify();
	static bool support_wait_with_timeout();
    };
}

#endif
