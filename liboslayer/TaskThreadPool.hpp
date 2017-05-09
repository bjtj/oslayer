#ifndef __TASK_THREAD_POOL_HPP__
#define __TASK_THREAD_POOL_HPP__

#include "Task.hpp"
#include "ThreadPool.hpp"
#include "AutoRef.hpp"

namespace UTIL {

	/**
	 * @brief task trhead pool
	 */
	class TaskThreadPool : public ThreadPool {
	private:
	public:
		TaskThreadPool(size_t count);
		virtual ~TaskThreadPool();
		void setTask(OS::AutoRef<Task> task);
		void setTaskWaitIfFull(OS::AutoRef<Task> task);
		void setTaskWaitIfFullWithTimeout(OS::AutoRef<Task> task, unsigned long timeout);
	};
}

#endif
