#ifndef __AUTO_LOCK_HPP__
#define __AUTO_LOCK_HPP__

#include "Semaphore.hpp"
#include "Mutex.hpp"
#include "Ref.hpp"

namespace OS {

	/**
     * @brief auto lock
     */
    class AutoLock {
    private:
        Ref<Semaphore> _sem;
		Ref<Mutex> _mutex;
    public:
        AutoLock(Ref<Semaphore> sem);
		AutoLock(Ref<Mutex> mutex);
        virtual ~AutoLock();
    };
}

#endif
