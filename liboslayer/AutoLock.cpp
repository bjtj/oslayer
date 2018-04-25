#include "AutoLock.hpp"

namespace osl {

	/**
     * @breif auto lock
     */
    
    AutoLock::AutoLock(Ref<Semaphore> sem) : _sem(sem) {
        _sem->wait();
    }
	AutoLock::AutoLock(Ref<Mutex> mutex) : _mutex(mutex) {
        _mutex->lock();
    }
    AutoLock::~AutoLock() {
		if (_sem.nil() == false) {
			_sem->post();
		}
		if (_mutex.nil() == false) {
			_mutex->unlock();
		}
    }

}
