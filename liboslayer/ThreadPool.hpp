#ifndef __THREAD_POOL_HPP__
#define __THREAD_POOL_HPP__

#include <deque>
#include "os.hpp"
#include "AutoRef.hpp"
#include "Task.hpp"
#include "Event.hpp"
#include "Thread.hpp"
#include "Observer.hpp"
#include "Semaphore.hpp"
#include "Pool.hpp"

namespace osl {

	/**
	 * @brief PoolThread
	 */
	class StatefulThread : public Thread, public Observable {
	private:
		AutoRef<Event> _evt;
		bool _triggered;
		bool _busy;
		AutoRef<Task> _task;
	public:
		StatefulThread();
		virtual ~StatefulThread();
		AutoRef<Task> & task();
		void preTask();
		void postTask();
		bool inBusy();
		void waitTillEnd();
		void loop();
		virtual void interrupt();
		virtual void onTask();
		virtual void run();
		void wakeup();
	};

	/**
	 *
	 */
	class ThreadPool : public Observer, public Observable {
	private:
		Pool<StatefulThread> _pool;
		bool _running;
		bool _finishing;
	public:
		ThreadPool(size_t poolSize);
		virtual ~ThreadPool();
        void init();
		void start();
		void stop();
		void collectIdleThreads();
		void collectThread(StatefulThread * thread);
		StatefulThread * acquire();
		void release(StatefulThread * thread);
		void enqueue(StatefulThread * thread);
		StatefulThread * dequeue();
		size_t freeCount();
		size_t workingCount();
		size_t capacity();
		bool finishing() const;
		virtual void onUpdate(Observable * target);
	};
}

#endif
