#include "ThreadPool.hpp"
#include <algorithm>

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
	 *
	 */

	StatefulThread::StatefulThread() : triggered(false), busy(false) {
	}
	StatefulThread::~StatefulThread() {
	}
	void StatefulThread::setBegin() {
		busy = true;
	}
	void StatefulThread::setEnd() {
		busy = false;
	}

	bool StatefulThread::inBusy() {
		return busy;
	}
	
	void StatefulThread::waitTillEnd() {
		while (busy) {
			idle(10);
		}
	}
	void StatefulThread::loop() {
		while (!interrupted()) {
			if (!triggered) {
				idle(10);
				continue;
			}
			triggered = false;
			
			setBegin();
			onTask();
			setEnd();
			
			notifyObservers();
		}
	}
	void StatefulThread::onTask() {
	}
	void StatefulThread::run() {
		loop();
	}
	void StatefulThread::setTrigger(bool trigger) {
		this->triggered = trigger;
	}
	bool StatefulThread::isTriggered() {
		return triggered;
	}

	/**
	 *
	 */
	ThreadPool::ThreadPool(size_t poolSize, InstanceCreator<StatefulThread*> & creator)
		: freeQueueLock(1), workingQueueLock(1), poolSize(poolSize), creator(creator), running(false) {
	}

	
	ThreadPool::~ThreadPool() {
		stop();
	}
    
    void ThreadPool::init() {
		
		workingQueue.clear();
		freeQueue.clear();
		
        for (size_t i = 0; i < poolSize; i++) {
			StatefulThread * t = creator.createInstance();
			t->addObserver(this);
			freeQueue.push_back(t);
        }
    }

	void ThreadPool::start() {

		if (!running) {
            
            init();

			for (deque<StatefulThread*>::iterator iter = freeQueue.begin(); iter != freeQueue.end(); iter++) {
				StatefulThread * thread = *iter;
				thread->start();
			}

			running = true;
		}

	}
	void ThreadPool::stop() {

		if (running) {

			vector<StatefulThread*> lst;
			workingQueueLock.wait();
			for (deque<StatefulThread*>::iterator iter = workingQueue.begin(); iter != workingQueue.end(); iter++) {
				StatefulThread * thread = *iter;
				thread->interrupt();
				lst.push_back(thread);
			}
			workingQueueLock.post();
			
			for (size_t i = 0; i < lst.size(); i++) {
				lst[i]->waitTillEnd();
			}
            
            for (deque<StatefulThread*>::iterator iter = freeQueue.begin(); iter != freeQueue.end(); iter++) {
                StatefulThread * thread = *iter;
                
                thread->interrupt();
                thread->wait();
                
                creator.releaseInstance(thread);
            }
            
            freeQueue.clear();
            workingQueue.clear();

			running = false;
		}
	}

	void ThreadPool::collectIdleThreads() {
		workingQueueLock.wait();
		for (deque<StatefulThread*>::iterator iter = workingQueue.begin(); iter != workingQueue.end();) {
			StatefulThread * thread = *iter;
			if (!thread->inBusy()) {
				iter = workingQueue.erase(iter);
				notifyObservers(thread);
				release(thread);
			} else {
				iter++;
			}
		}
		workingQueueLock.post();
	}

	void ThreadPool::collectThread(StatefulThread * thread) {
		workingQueueLock.wait();
		if (find(workingQueue.begin(), workingQueue.end(), thread) != workingQueue.end()) {
			workingQueue.erase(find(workingQueue.begin(), workingQueue.end(), thread));
		}
		notifyObservers(thread);
		release(thread);
		workingQueueLock.post();
	}

	StatefulThread * ThreadPool::acquire() {
		freeQueueLock.wait();
		StatefulThread * thread = freeQueue.size() > 0 ? freeQueue.front() : NULL;
		if (thread) {
			freeQueue.pop_front();
		}
		freeQueueLock.post();

		return thread;
	}
    
	void ThreadPool::release(StatefulThread * thread) {
		freeQueueLock.wait();
		if (std::find(freeQueue.begin(), freeQueue.end(), thread) == freeQueue.end()) {
			freeQueue.push_back(thread);
		}
		freeQueueLock.post();
	}
    
	void ThreadPool::enqueue(StatefulThread * thread) {
		workingQueueLock.wait();
		if (find(workingQueue.begin(), workingQueue.end(), thread) == workingQueue.end()) {
			workingQueue.push_back(thread);
		}
		thread->setTrigger(true);
		workingQueueLock.post();
	}
    
	StatefulThread * ThreadPool::dequeue() {
		workingQueueLock.wait();
		StatefulThread * thread = workingQueue.size() > 0 ? workingQueue.front() : NULL;
		if (thread) {
			workingQueue.pop_front();
		}
		workingQueueLock.post();
		return thread;
	}

	size_t ThreadPool::freeCount() {
		return freeQueue.size();
	}
	
	size_t ThreadPool::workingCount() {
		return workingQueue.size();
	}
	
	size_t ThreadPool::capacity() {
		return poolSize;
	}
	
	void ThreadPool::onUpdate(Observable * target) {
		StatefulThread * t = (StatefulThread*)target;
		collectThread(t);
	}
}
