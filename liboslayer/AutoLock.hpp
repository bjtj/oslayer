#ifndef __AUTO_LOCK_HPP__
#define __AUTO_LOCK_HPP__

#include "Semaphore.hpp"
#include "Mutex.hpp"
#include "Ref.hpp"

#define auto_lock(tok) for(AutoLock __lx(tok);__lx.flag() == false; __lx.flag() = true)

namespace osl {

    /**
     * @brief auto lock
     */
    class AutoLock {
    private:
	bool _flag;
        Ref<Semaphore> _sem;
	Ref<Mutex> _mutex;
    public:
        AutoLock(Ref<Semaphore> sem);
	AutoLock(Ref<Mutex> mutex);
        virtual ~AutoLock();
	bool & flag();
    };
}

#endif
