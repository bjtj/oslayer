#ifndef __AUTO_REF_HPP__
#define __AUTO_REF_HPP__

#include "os.hpp"
#include "Mutex.hpp"

namespace OS {

	/**
	 * shared reference counter
	 */
    class SharedRefCounter {
    private:
        int _count;
        Mutex _mutex;
    public:
        SharedRefCounter();
        virtual ~SharedRefCounter();
		int ref();
		int unref();
		int ref_count();
    };

	/**
	 * auto ref
	 */
    template <typename T>
    class AutoRef {
    private:
        SharedRefCounter * _counter;
        T * _t;
    public:
        explicit AutoRef();
		explicit AutoRef(T * _t);
        AutoRef(const AutoRef<T> & other);
        virtual ~AutoRef();        
        AutoRef<T> & operator= (T * t);
        AutoRef<T> & operator= (const AutoRef<T> & other);
        T & operator* ();
        T * operator-> () const;
        T * operator& ();
		bool operator== (const AutoRef<T> & other) const;
        int ref_count();
		bool nil() const;
    private:
        void _retain();
        void _release();
        void _copy(const AutoRef<T> & other);
    };


	template <typename T>
	AutoRef<T>::AutoRef() : _counter(NULL), _t(NULL) {
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(T * _t) : _counter(NULL), _t(_t) {
        _retain();
    }
        
	template <typename T>
    AutoRef<T>::AutoRef(const AutoRef<T> & other) : _counter(NULL), _t(NULL) {
        _copy(other);
    }
        
	template <typename T>
    AutoRef<T>::~AutoRef() {
        _release();
    }
    
	template <typename T>
    AutoRef<T> & AutoRef<T>::operator= (T * t) {
        _release();
        _counter = NULL;
        _t = t;
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
    int AutoRef<T>::ref_count() {
        return _counter ? _counter->ref_count() : -1;
    }

	template <typename T>
    bool AutoRef<T>::nil() const {
        return _t == NULL;
    }
        
	template <typename T>
    void AutoRef<T>::_retain() {
        if (_t == NULL) {
			return;
		}
		if (!_counter) {
			_counter = new SharedRefCounter;
		}
		_counter->ref();
    }
        
	template <typename T>
    void AutoRef<T>::_release() {
        if (_counter && _counter->unref() == 0) {
            if (_counter) {
				delete _counter;
				_counter = NULL;
			}
			if (_t) {
				delete _t;
				_t = NULL;
			}
        }
    }
        
	template <typename T>
    void AutoRef<T>::_copy(const AutoRef<T> & other) {
        if (_t == other._t) {
			return;
		}

		SharedRefCounter * x_counter = _counter;
		T * x_t = _t;

		_counter = other._counter;
		_t = other._t;
		_retain();

		//
		if (x_counter && x_counter->unref() == 0) {
            if (x_counter) {
				delete x_counter;
				x_counter = NULL;
			}
			if (x_t) {
				delete x_t;
				x_t = NULL;
			}
        }
    }

	/**
	 * not allow void
	 */
    template<>
    class AutoRef<void> {
    private:
    public:
    };
    
}

#endif
