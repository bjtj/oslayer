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
	class FlaggableThread : public OS::Thread, public Observable {
	private:
		bool flag;
	public:
		FlaggableThread(bool initialFlag);
		virtual ~FlaggableThread();
		bool flagged();
		void setFlag(bool flag);
	};

	/**
	 *
	 */
	class ThreadPool : public Observer, public Observable {
	private:
		OS::Semaphore freeQueueLock;
		std::deque<FlaggableThread*> freeQueue;
		OS::Semaphore workingQueueLock;
		std::deque<FlaggableThread*> workingQueue;
		size_t poolSize;
		InstanceCreator<FlaggableThread*> & creator;
		bool running;

	public:
		ThreadPool(size_t poolSize, InstanceCreator<FlaggableThread*> & creator);
		virtual ~ThreadPool();
        void init();
		void start();
		void stop();
		void collectUnflaggedThreads();
		FlaggableThread * acquire();
		void release(FlaggableThread * thread);
		void enqueue(FlaggableThread * thread);
		FlaggableThread * dequeue();
		size_t freeCount();
		size_t workingCount();
		virtual void update(Observable * target);
	};

}

#endif
