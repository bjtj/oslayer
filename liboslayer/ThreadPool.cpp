#include "ThreadPool.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	
	ThreadPool::ThreadPool(size_t poolSize, InstanceCreator<FlaggableThread*> & creator) : freeQueueLock(1), workingQueueLock(1), poolSize(poolSize), creator(creator), running(false) {

        init();
	}

	
	ThreadPool::~ThreadPool() {
		stop();
	}
    
    void ThreadPool::init() {
        stop();
        for (size_t i = 0; i < poolSize; i++) {
            freeQueue.push_back(creator.createInstance());
        }
    }

	void ThreadPool::start() {

		if (!running) {
            
            init();

			for (deque<FlaggableThread*>::const_iterator iter = freeQueue.begin(); iter != freeQueue.end(); iter++) {
				FlaggableThread * thread = *iter;
				thread->start();
			}

			running = true;
		}

	}
	void ThreadPool::stop() {

		if (running) {

			workingQueueLock.wait();
			for (deque<FlaggableThread*>::const_iterator iter = workingQueue.begin(); iter != workingQueue.end(); iter++) {
				FlaggableThread * thread = *iter;
				thread->setFlag(false);
			}
			workingQueueLock.post();
            
            collectUnflaggedThreads();
            
            for (deque<FlaggableThread*>::const_iterator iter = freeQueue.begin(); iter != freeQueue.end(); iter++) {
                FlaggableThread * thread = *iter;
                
                thread->interrupt();
                thread->setFlag(false);
                thread->join();
                
                creator.releaseInstance(thread);
            }
            
            freeQueue.clear();
            workingQueue.clear();

			running = false;
		}
	}

	void ThreadPool::collectUnflaggedThreads() {

		workingQueueLock.wait();
		for (deque<FlaggableThread*>::iterator iter = workingQueue.begin(); iter != workingQueue.end();) {
			FlaggableThread * thread = *iter;
			if (!thread->flagged()) {
				release(thread);
				iter = workingQueue.erase(iter);
			} else {
				iter++;
			}
		}
		workingQueueLock.post();

	}

	FlaggableThread * ThreadPool::acquire() {

		freeQueueLock.wait();
		FlaggableThread * thread = freeQueue.size() > 0 ? freeQueue.front() : NULL;
		if (thread) {
			thread->setFlag(false);
			freeQueue.pop_front();
		}
		freeQueueLock.post();

		return thread;
	}
    
	void ThreadPool::release(FlaggableThread * thread) {
		freeQueueLock.wait();
		freeQueue.push_back(thread);
		freeQueueLock.post();
	}
    
	void ThreadPool::enqueue(FlaggableThread * thread) {
		workingQueueLock.wait();
		workingQueue.push_back(thread);
		thread->setFlag(true);
		workingQueueLock.post();
	}
    
	FlaggableThread * ThreadPool::dequeue() {
		workingQueueLock.wait();
		FlaggableThread * thread = workingQueue.size() > 0 ? workingQueue.front() : NULL;
		if (thread) {
			workingQueue.pop_front();
		}
		workingQueueLock.post();
		return thread;
	}

}