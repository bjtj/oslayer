#ifndef __POOL_HPP__
#define __POOL_HPP__

#include "os.hpp"
#include <deque>
#include "Observer.hpp"

namespace UTIL {

	class PoolThread;

	/**
	 * @brief ThreadMaker
	 */
	template<typename T>
	class InstanceCreator {
	private:
	public:
		InstanceCreator() {}
		virtual ~InstanceCreator() {}

		virtual T createInstance() = 0;
		virtual void releaseInstance(T inst) = 0;
	};

	/**
	 * @brief PoolThread
	 */
	class StatefulThread : public OS::Thread, public Observable {
	private:
		bool triggered;
		bool busy;
	public:
		StatefulThread();
		virtual ~StatefulThread();
		void setBegin();
		void setEnd();
		bool inBusy();
		void waitTillEnd();
		void loop();
		virtual void onTask();
		virtual void run();
		void setTrigger(bool trigger);
		bool isTriggered();
	};

	/**
	 *
	 */
	class ThreadPool : public Observer, public Observable {
	private:
		OS::Semaphore freeQueueLock;
		std::deque<StatefulThread*> freeQueue;
		OS::Semaphore workingQueueLock;
		std::deque<StatefulThread*> workingQueue;
		size_t poolSize;
		InstanceCreator<StatefulThread*> & creator;
		bool running;

	public:
		ThreadPool(size_t poolSize, InstanceCreator<StatefulThread*> & creator);
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
		virtual void onUpdate(Observable * target);
	};
}

#endif
