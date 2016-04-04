#ifndef __TASK_THREAD_POOL_HPP__
#define __TASK_THREAD_POOL_HPP__

#include "Task.hpp"
#include "ThreadPool.hpp"
#include "AutoRef.hpp"

namespace UTIL {

	/**
	 * @brief task thread
	 */
	class TaskThread : public FlaggableThread {
	private:
		AutoRef<Task> task;
	public:
		TaskThread();
		virtual ~TaskThread();
		void setTask(AutoRef<Task> task);
		virtual void run();
	};


	/**
	 * @brief task trhead pool
	 */
	class TaskThreadPool : public ThreadPool {
	private:
	public:
		TaskThreadPool(size_t count);
		virtual ~TaskThreadPool();
		void setTask(AutoRef<Task> task);
	};
	
}

#endif
