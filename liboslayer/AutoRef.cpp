#include "AutoRef.hpp"

namespace OS {
	
    SharedRefCounter::SharedRefCounter() : _count(0) {
    }
    SharedRefCounter::~SharedRefCounter() {
    }

	int SharedRefCounter::ref() {
		_mutex.lock();
		_count++;
		_mutex.unlock();
		return _count;
	}
	int SharedRefCounter::unref() {
		_mutex.lock();
		if (_count > 0) {
			_count--;
		}
		_mutex.unlock();
		return _count;
	}
	int SharedRefCounter::ref_count() {
		return _count;
	}
}
