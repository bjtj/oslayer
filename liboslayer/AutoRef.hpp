#ifndef __AUTO_REF_HPP__
#define __AUTO_REF_HPP__

#include "os.hpp"

namespace OS {

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
        explicit AutoRef();
		explicit AutoRef(T * _t);
        AutoRef(const AutoRef<T> & other);
        virtual ~AutoRef();        
        AutoRef<T> & operator= (T * _t);        
        AutoRef<T> & operator= (const AutoRef<T> & other);
        T & operator* ();
        T * operator-> () const;
        T * operator& ();
		bool operator== (const AutoRef<T> & other) const;
        int count();
		bool nil() const;
    private:
        void _retain();
        void _release();
        void _destroy();
        void _copy(const AutoRef<T> & other);
    };


	template <typename T>
	AutoRef<T>::AutoRef() : counter(NULL), _t(NULL) {
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(T * _t) : counter(NULL), _t(_t) {
        _retain();
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(const AutoRef<T> & other) : counter(NULL), _t(NULL) {
        _copy(other);
    }
        
	template <typename T>
    AutoRef<T>::~AutoRef() {
        _release();
    }
    
	template <typename T>
    AutoRef<T> & AutoRef<T>::operator= (T * _t) {
        _release();
        counter = NULL;
        this->_t = _t;
        _retain();
        return *this;
    }
	
	template <typename T>
    AutoRef<T> & AutoRef<T>::operator= (const AutoRef<T> & other) {
        _copy(other);
        return *this;
    }
	
	template <typename T>
    T & AutoRef<T>::operator* () {
        if (!_t) {
            throw OS::NullException("null exception (operator*)");
        }
        return *_t;
    }

	template <typename T>
    T * AutoRef<T>::operator-> () const {
        if (!_t) {
            throw OS::NullException("null exception (operator->)");
        }
        return _t;
    }
    
	template <typename T>
    T * AutoRef<T>::operator& () {
        return _t;
    }

	template <typename T>
	bool AutoRef<T>::operator== (const AutoRef<T> & other) const {
		return _t == other._t;
	}
        
	template <typename T>
    int AutoRef<T>::count() {
        return counter ? counter->count() : -1;
    }

	template <typename T>
    bool AutoRef<T>::nil() const {
        return _t == NULL;
    }
        
	template <typename T>
    void AutoRef<T>::_retain() {
        if (_t) {
            if (!counter) {
                counter = new SharedCounter;
            }
            counter->countUp();
        }
    }
        
	template <typename T>
    void AutoRef<T>::_release() {
        if (counter && counter->countDownAndCheckZero()) {
            _destroy();
        }
    }
        
	template <typename T>
    void AutoRef<T>::_destroy() {
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
    void AutoRef<T>::_copy(const AutoRef<T> & other) {
        if (_t != other._t) {
            _release();
            counter = other.counter;
            _t = other._t;
            _retain();
        }
    }
    
    template<>
    class AutoRef<void> {
    private:
    public:
    };
    
}

#endif
