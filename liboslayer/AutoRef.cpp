#include "AutoRef.hpp"
#include "AutoLock.hpp"

namespace OS {

	/**
	 * 
	 */
    RefCounter::RefCounter() : _count(0) {
    }

	RefCounter::RefCounter(int count) : _count(count) {
    }
	
    RefCounter::~RefCounter() {
    }

	int RefCounter::ref() {
		_count++;
		return _count;
	}
	
	int RefCounter::unref() {
		if (_count > 0) {
			_count--;
		}
		return _count;
	}
	
	int & RefCounter::ref_count() {
		return _count;
	}

	/**
	 * 
	 */
	SharedRefCounter::SharedRefCounter() {
    }

	SharedRefCounter::SharedRefCounter(int count) : RefCounter(count) {
    }
	
    SharedRefCounter::~SharedRefCounter() {
    }

	int SharedRefCounter::ref() {
		AutoLock _lock(Ref<Mutex>(&_mutex));
		return RefCounter::ref();
	}
	
	int SharedRefCounter::unref() {
		AutoLock _lock(Ref<Mutex>(&_mutex));
		return RefCounter::unref();
	}
}
