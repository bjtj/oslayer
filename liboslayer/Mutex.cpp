#include "Mutex.hpp"

namespace OS {

	Mutex::Mutex() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_init(&_mutex, NULL) != 0) {
			throw Exception("pthread_mutex_init() error");
		}
#elif defined(USE_MS_WIN)
		_mutex = CreateMutex(NULL, FALSE, NULL);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}
	
	Mutex::~Mutex() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_destroy(&_mutex) != 0) {
            // do not throw
            // https://stackoverflow.com/a/130123
			// - throw Exception("pthread_mutex_destroy() error");
		}
#elif defined(USE_MS_WIN)
		CloseHandle(_mutex);
#else
        // do not throw and not reachable by constructor
		// - throw NotSupportedPlatformException("not supported platform");
#endif
	}
	
	void Mutex::lock() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_lock(&_mutex) != 0) {
			throw Exception("pthread_mutex_lock() failed");
		}
#elif defined(USE_MS_WIN)
		// TODO: https://msdn.microsoft.com/ko-kr/library/windows/desktop/ms686927(v=vs.85).aspx
		WaitForSingleObject(_mutex, INFINITE);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}
	
	void Mutex::unlock() {
#if defined(USE_PTHREAD)
		if (pthread_mutex_unlock(&_mutex) != 0) {
			throw Exception("pthread_mutex_unlock() failed");
		}
#elif defined(USE_MS_WIN)
		ReleaseMutex(_mutex);
#else
		throw NotSupportedPlatformException("not supported platform");
#endif
	}

}
