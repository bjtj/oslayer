#include "TaskThreadPool.hpp"
#include "os.hpp"

namespace osl {

	

    /**
     * @brief
     */

    TaskThreadPool::TaskThreadPool(size_t count) : ThreadPool(count) {
    }
    TaskThreadPool::~TaskThreadPool() {
    }
    void TaskThreadPool::setTask(AutoRef<Task> task) {
	StatefulThread * thread = acquire();
	if (!thread) {
	    throw Exception("no available thread");
	}
	thread->task() = task;
	enqueue(thread);
    }
    void TaskThreadPool::setTaskWaitIfFull(AutoRef<Task> task) {
	StatefulThread * thread = NULL;
	while ((thread = acquire()) == NULL) {
	    idle(10);
	}
	thread->task() = task;
	enqueue(thread);
    }
    void TaskThreadPool::setTaskWaitIfFullWithTimeout(AutoRef<Task> task, unsigned long timeout) {
	StatefulThread * thread = NULL;
	unsigned long tick = tick_milli();
	while ((thread = acquire()) == NULL) {
	    if (tick_milli() - tick >= timeout) {
		throw Exception("timeout occurred");
	    }
	    idle(10);
	}
	thread->task() = task;
	enqueue(thread);
    }
}
