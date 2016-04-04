#ifndef __POOL_HPP__
#define __POOL_HPP__

#include "os.hpp"
#include <deque>

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
	class FlaggableThread : public OS::Thread {
	private:
		bool flag;
	public:
		FlaggableThread(bool initialFlag) : flag(initialFlag) {}
		virtual ~FlaggableThread() {}

		bool flagged() {return flag;}
		void setFlag(bool flag) {this->flag = flag;}
	};

	/**
	 *
	 */
	class ThreadPool {
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
	};

}

#endif
