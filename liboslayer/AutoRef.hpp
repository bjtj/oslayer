#ifndef __AUTO_REF_HPP__
#define __AUTO_REF_HPP__

#include "os.hpp"

namespace UTIL {

    class SharedCounter {
    private:
        int _count;
    public:
        SharedCounter();
        virtual ~SharedCounter();
        void countUp();
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
        
        AutoRef() : counter(NULL), _t(NULL) {
        }
        AutoRef(T * _t) : counter(NULL), _t(_t) {
            retain();
        }
        AutoRef(const AutoRef & other) : counter(NULL), _t(NULL) {
            copy(other);
        }
        virtual ~AutoRef() {
            release();
        }
        AutoRef<T> & operator= (T * _t) {
            reset();
            this->_t = _t;
            retain();
            return *this;
        }
        AutoRef<T> & operator= (const AutoRef<T> & other) {
            copy(other);
            return *this;
        }
        
        T & operator* () {
            if (!_t) {
                throw OS::NullException("null exception", -1, 0);
            }
            return *_t;
        }
        T * operator-> () {
            if (!_t) {
                throw OS::NullException("null exception", -1, 0);
            }
            return _t;
        }
        T * operator& () {
            return _t;
        }
        
        int count() {
            return counter ? counter->count() : -1;
        }
        
        bool empty() {
            return _t == NULL;
        }
        
    private:
        
        void retain() {
            if (!counter) {
                counter = new SharedCounter;
            }
            counter->countUp();
        }
        void release() {
            if (counter) {
                counter->countDown();
                if (counter->zero()) {
                    destroy();
                }
            }
        }
        void destroy() {
            if (counter) {
                delete counter;
                counter = NULL;
            }
            if (_t) {
                delete _t;
                _t = NULL;
            }
        }
        void copy(const AutoRef<T> & other) {
            if (_t != other._t) {
                release();
                counter = other.counter;
                _t = other._t;
                retain();
            }
        }
        void reset() {
            release();
            destroy();
        }
        
    };
    
    template<>
    class AutoRef<void> {
    private:
    public:
    };
    
}

#endif
