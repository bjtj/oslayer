#ifndef __AUTO_REF_HPP__
#define __AUTO_REF_HPP__

#include "os.hpp"

namespace UTIL {

    class SharedCounter {
    private:
        int _count;
        OS::Semaphore sem;
    public:
        SharedCounter();
        virtual ~SharedCounter();
        void countUp();
        bool countDownAndCheckZero();
        void countDown();
        bool zero();
        int count();
    };
    
    template <typename T>
    class AutoRef {
    private:
        
        SharedCounter * counter;
        T * _t;
        
    public:
        
        AutoRef();
		AutoRef(T * _t);
        AutoRef(const AutoRef & other);
        virtual ~AutoRef();        
        AutoRef<T> & operator= (T * _t);        
        AutoRef<T> & operator= (const AutoRef<T> & other);
        T & operator* ();
        T * operator-> () const;
        T * operator& ();
        int count();
        bool empty() const;

    private:

        void retain();
        void release();
        void destroy();
        void copy(const AutoRef<T> & other);
        
    };


	template <typename T>
	AutoRef<T>::AutoRef() : counter(NULL), _t(NULL) {
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(T * _t) : counter(NULL), _t(_t) {
        retain();
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(const AutoRef & other) : counter(NULL), _t(NULL) {
        copy(other);
    }
        
	template <typename T>
    AutoRef<T>::~AutoRef() {
        release();
    }
    
	template <typename T>
    AutoRef<T> & AutoRef<T>::operator= (T * _t) {
        release();
        counter = NULL;
        this->_t = _t;
        retain();
        return *this;
    }
	
	template <typename T>
    AutoRef<T> & AutoRef<T>::operator= (const AutoRef<T> & other) {
        copy(other);
        return *this;
    }
	
	template <typename T>
    T & AutoRef<T>::operator* () {
        if (!_t) {
            throw OS::NullException("null exception (operator*)", -1, 0);
        }
        return *_t;
    }

	template <typename T>
    T * AutoRef<T>::operator-> () const {
        if (!_t) {
            throw OS::NullException("null exception (operator->)", -1, 0);
        }
        return _t;
    }
    
	template <typename T>
    T * AutoRef<T>::operator& () {
        return _t;
    }
        
	template <typename T>
    int AutoRef<T>::count() {
        return counter ? counter->count() : -1;
    }
        
	template <typename T>
    bool AutoRef<T>::empty() const {
        return _t == NULL;
    }
        
	template <typename T>
    void AutoRef<T>::retain() {
        if (_t) {
            if (!counter) {
                counter = new SharedCounter;
            }
            counter->countUp();
        }
    }
        
	template <typename T>
    void AutoRef<T>::release() {
        if (counter && counter->countDownAndCheckZero()) {
            destroy();
        }
    }
        
	template <typename T>
    void AutoRef<T>::destroy() {
        if (counter) {
            delete counter;
            counter = NULL;
        }
        if (_t) {
            delete _t;
            _t = NULL;
        }
    }
        
	template <typename T>
    void AutoRef<T>::copy(const AutoRef<T> & other) {
        if (_t != other._t) {
            release();
            counter = other.counter;
            _t = other._t;
            retain();
        }
    }
    
    template<>
    class AutoRef<void> {
    private:
    public:
    };
    
}

#endif
