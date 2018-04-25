#include "Event.hpp"

namespace osl {

	Event::Event() {
#if defined(USE_APPLE_STD)
        bool _mutex_init = false;
        try {
            if (pthread_mutex_init(&_mutex, NULL) != 0) {
                throw Exception("pthread_mutex_init() error");
            }
            _mutex_init = true;
            if (pthread_cond_init(&_cond, NULL) != 0) {
                throw Exception("pthread_cond_init() error");
            }
        } catch (Exception e) {
            if (_mutex_init) {
                if (pthread_mutex_destroy(&_mutex) != 0) {
                    // pthread_mutex_destroy() error
                }
            }
            throw e;
        }
#elif defined(USE_PTHREAD)
		bool _mutex_init = false;
		bool _condattr_init = false;
		pthread_condattr_t attr;
		try {
			if (pthread_mutex_init(&_mutex, NULL) != 0) {
				throw Exception("pthread_mutex_init() error");
			}
			_mutex_init = true;
			if (pthread_condattr_init(&attr) != 0) {
				throw Exception("pthread_condattr_init() error");
			}
			_condattr_init = true;
			if (pthread_condattr_setclock(&attr, CLOCK_MONOTONIC) != 0) {
				throw Exception("pthread_condattr_setclock() error");
			}
			if (pthread_cond_init(&_cond, &attr) != 0) {
				throw Exception("pthread_cond_init() error");
			}
			if (pthread_condattr_destroy(&attr) != 0) {
				// pthread_condattr_destroy() error
			}
		} catch (Exception e) {
			if (_mutex_init) {
				if (pthread_mutex_destroy(&_mutex) != 0) {
					// pthread_mutex_destroy() error
				}
			}
			if (_condattr_init) {
				if (pthread_condattr_destroy(&attr) != 0) {
					// pthread_condattr_destroy() error
				}
			}
			throw e;
		}
#elif defined(USE_MS_WIN)
		_mutex = CreateMutex(NULL, FALSE, NULL);
		_evt = CreateEvent(NULL, FALSE, FALSE, NULL);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}
	
    Event::~Event() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_destroy(&_mutex) != 0) {
			// pthread_mutex_destroy() error
		}
		if (pthread_cond_destroy(&_cond) != 0) {
			// pthread_cond_destroy() error
		}
#elif defined(USE_MS_WIN)
		CloseHandle(_evt);
		CloseHandle(_mutex);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}

	void Event::lock() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_lock(&_mutex) != 0) {
			throw Exception("pthread_mutex_lock() failed");
		}
#elif defined(USE_MS_WIN)
		// TODO: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms686927(v=vs.85).aspx
		WaitForSingleObject(_mutex, INFINITE);
#else
		throw NotSupportedPlatformException("Event::lock() is not supported on platform");
#endif
	}
	
	void Event::unlock() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_unlock(&_mutex) != 0) {
			throw Exception("pthread_mutex_lock() failed");
		}
#elif defined(USE_MS_WIN)
		ReleaseMutex(_mutex);
#else
		throw NotSupportedPlatformException("Event::unlock() is not supported on this platform");
#endif
	}
	
	void Event::wait() {
		wait(0);
	}

	void Event::wait(unsigned long timeout) {
#if defined(USE_APPLE_STD)
        if (timeout > 0) {
            throw NotSupportedPlatformException("Event::wait() with timeout is not supported on this platform");
        }
        pthread_cond_wait(&_cond, &_mutex);
#elif defined(USE_PTHREAD)
		if (timeout == 0) {
			pthread_cond_wait(&_cond, &_mutex);
		} else {
			struct timespec spec;
			clock_gettime(CLOCK_MONOTONIC, &spec);
			spec.tv_sec += (timeout / 1000);
			spec.tv_nsec += ((timeout % 1000) * 1000000);
			int ret = pthread_cond_timedwait(&_cond, &_mutex, &spec);
			if (ret != 0) {
				switch (ret) {
				case ETIMEDOUT:
					throw TimeoutException("timeout");
				default:
					throw Exception("unknown error");
				}
			}
		}
#elif defined(USE_MS_WIN)
		unlock();
		DWORD ret;
		if (timeout == 0) {
			ret = WaitForSingleObject(_evt, INFINITE);
		} else {
			ret = WaitForSingleObject(_evt, timeout);
		}		
		lock();
		switch (ret) {
		case WAIT_OBJECT_0:
			// good
			break;
		case WAIT_ABANDONED:
			break;
		case WAIT_TIMEOUT:
			throw TimeoutException("timeout");
		case WAIT_FAILED:
		default:
			throw Exception("WaitForSingleObject() failed");
		}
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}	
	
	void Event::notify() {
#if defined(USE_PTHREAD)
		pthread_cond_signal(&_cond);
#elif defined(USE_MS_WIN)
		SetEvent(_evt);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}

	bool Event::support_wait_with_timeout() {
#if defined(USE_APPLE_STD)
		return false;
#elif defined(USE_PTHREAD)
		return true;
#elif defined(USE_MS_WIN)
		return true;
#else
		return false;
#endif
	}
}
