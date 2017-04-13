#ifndef __EVENT_HPP__
#define __EVENT_HPP__

#include "os.hpp"

namespace OS {

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
#else
		// not support
#endif
	private:
		Event(const Event & other); // not allow
		Event & operator=(const Event & other); // not allow
	public:
		Event();
		virtual ~Event();
		void lock();
		void unlock();
		void wait();
		void wait(unsigned long timeout);
		void notify();
	};
}

#endif
