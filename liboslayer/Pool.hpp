#ifndef __POOL_HPP__
#define __POOL_HPP__

#include <algorithm>
#include <deque>
#include "Mutex.hpp"

namespace osl {

    template <typename T>
    class Pool {
    private:
	Mutex _avail_lock;
	Mutex _work_lock;
	std::deque<T*> _avails;
	std::deque<T*> _works;
	size_t _size;
    public:
	Pool(size_t size) : _size(size) {
	    alloc();
	}
	virtual ~Pool() {
	    clear();
	}
    protected:
	void alloc() {
	    for (size_t i = 0; i < _size; i++) {
		T * t = new T;
		_avails.push_back(t);
	    }
	}
	void clear() {
	    for (typename std::deque<T*>::iterator iter = _avails.begin(); iter != _avails.end(); iter++) {
		delete *iter;
	    }
	    for (typename std::deque<T*>::iterator iter = _works.begin(); iter != _works.end(); iter++) {
		delete *iter;
	    }
	    _avails.clear();
	    _works.clear();
	}
    public:
	void lock_avail() {
	    _avail_lock.lock();
	}
	void unlock_avail() {
	    _avail_lock.unlock();
	}
	void lock_work() {
	    _work_lock.lock();
	}
	void unlock_work() {
	    _work_lock.unlock();
	}
	T * acquire() {
	    _avail_lock.lock();
	    T * item = _avails.size() > 0 ? _avails.front() : NULL;
	    if (item) {
		_avails.pop_front();
	    }
	    _avail_lock.unlock();
	    return item;
	}
	void release(T * t) {
	    _avail_lock.lock();
	    if (std::find(_avails.begin(), _avails.end(), t) == _avails.end()) {
		_avails.push_back(t);
	    }
	    _avail_lock.unlock();
	}
	void enqueue(T * t) {
	    _work_lock.lock();
	    if (std::find(_works.begin(), _works.end(), t) == _works.end()) {
		_works.push_back(t);
	    }
	    _work_lock.unlock();
	}
	T * dequeue() {
	    _work_lock.lock();
	    T * item = _works.size() > 0 ? _works.front() : NULL;
	    if (item) {
		_works.pop_front();
	    }
	    _work_lock.unlock();
	    return item;
	}
	void rest(T * t) {
	    _work_lock.lock();
	    for (typename std::deque<T*>::iterator iter = _works.begin(); iter != _works.end(); iter++) {
		if (*iter == t) {
		    _works.erase(iter);
		    break;
		}
	    }
	    _work_lock.unlock();
	}
	size_t available() const {
	    return _avails.size();
	}
	size_t busy() const {
	    return _works.size();
	}
	std::deque<T*> & avail_queue() {
	    return _avails;
	}
	std::deque<T*> & work_queue() {
	    return _works;
	}
	size_t size() const {
	    return _size;
	}
    };
}

#endif
