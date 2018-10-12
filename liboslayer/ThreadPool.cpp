#include "ThreadPool.hpp"
#include <algorithm>

namespace osl {

	using namespace std;
	

	/**
	 *
	 */

	StatefulThread::StatefulThread()
		: _evt(new Event), _triggered(false), _busy(false) {
	}
	
	StatefulThread::~StatefulThread() {
	}

	AutoRef<Task> & StatefulThread::task() {
		return _task;
	}
	
	void StatefulThread::preTask() {
		_busy = true;
	}
	
	void StatefulThread::postTask() {
		_busy = false;
	}

	bool StatefulThread::inBusy() {
		return _busy;
	}
	
	void StatefulThread::waitTillEnd() {
		while (_busy) {
			idle(10);
		}
	}
	void StatefulThread::loop() {
		while (!interrupted()) {
			bool _cont = false;
			_evt->lock();
			if (!_triggered) {
				_evt->wait();
				_cont = !_triggered;
			}
			_triggered = false;
			_evt->unlock();
			if (_cont) {
				continue;
			}
			preTask();
			onTask();
			postTask();
			notifyObservers();
		}
	}
	
	void StatefulThread::interrupt() {
		if (_task.nil() == false) {
			_task->cancel();
		}
		Thread::interrupt();
		_evt->notify();
	}
	
	void StatefulThread::onTask() {
		if (_task.nil() == false) {
			_task->run();
		}
	}
	
	void StatefulThread::run() {
		loop();
	}

	void StatefulThread::wakeup() {
		_triggered = true;
		_evt->lock();
		_evt->notify();
		_evt->unlock();
	}

	/**
	 *
	 */
	ThreadPool::ThreadPool(size_t poolSize)
		: _pool(poolSize), _running(false), _finishing(false) {
	}
	
	ThreadPool::~ThreadPool() {
		stop();
	}
    
    void ThreadPool::init() {
		_finishing = false;
		deque<StatefulThread*> & qu = _pool.avail_queue();
		for (deque<StatefulThread*>::iterator iter = qu.begin(); iter != qu.end(); iter++) {
			(*iter)->addObserver(this);
		}
    }

	void ThreadPool::start() {
		if (_running == false) {
			init();
			_pool.lock_avail();
			deque<StatefulThread*> & qu = _pool.avail_queue();
			for (deque<StatefulThread*>::iterator iter = qu.begin(); iter != qu.end(); iter++) {
				(*iter)->start();
			}
			_pool.unlock_avail();
			_running = true;
		}
	}
	
	void ThreadPool::stop() {
		if (_running == true) {
			_finishing = true;
			StatefulThread * thread;
			while ((thread = _pool.dequeue())) {
				thread->interrupt();
				_pool.release(thread);
			}
			deque<StatefulThread*> & qu = _pool.avail_queue();
			// TODO: WIN32 have problem in this code - 'deque iterator not incrementable'
			/*for (deque<StatefulThread*>::iterator iter = qu.begin(); iter != qu.end(); iter++) {
				(*iter)->interrupt();
				(*iter)->wait();
			}*/
			for (size_t i = 0; i < qu.size(); i++) {
				qu[i]->interrupt();
				qu[i]->join();
			}
			_running = false;
		}
	}

	void ThreadPool::collectIdleThreads() {
		_pool.lock_work();
		deque<StatefulThread*> & qu = _pool.work_queue();
		for (deque<StatefulThread*>::iterator iter = qu.begin(); iter != qu.end();) {
			if ((*iter)->inBusy() == false) {
				notifyObservers(*iter);
				_pool.release(*iter);
				iter = qu.erase(iter);
			} else {
				iter++;
			}
		}
		_pool.unlock_work();
	}

	void ThreadPool::collectThread(StatefulThread * thread) {
		_pool.rest(thread);
		notifyObservers(thread);
		_pool.release(thread);
	}

	StatefulThread * ThreadPool::acquire() {
		return _pool.acquire();
	}
    
	void ThreadPool::release(StatefulThread * thread) {
		_pool.release(thread);
	}
    
	void ThreadPool::enqueue(StatefulThread * thread) {
		_pool.enqueue(thread);
		thread->wakeup();
	}
    
	StatefulThread * ThreadPool::dequeue() {
		return _pool.dequeue();
	}

	size_t ThreadPool::freeCount() {
		return _pool.available();
	}
	
	size_t ThreadPool::workingCount() {
		return _pool.busy();
	}
	
	size_t ThreadPool::capacity() {
		return _pool.size();
	}

	bool ThreadPool::finishing() const {
		return _finishing;
	}
	
	void ThreadPool::onUpdate(Observable * target) {
		if (_finishing) {
			return;
		}
		StatefulThread * t = (StatefulThread*)target;
		collectThread(t);
	}
}
