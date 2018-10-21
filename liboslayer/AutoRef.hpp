#ifndef __AUTO_REF_HPP__
#define __AUTO_REF_HPP__

#include "os.hpp"
#include "Mutex.hpp"
#include "Ref.hpp"

namespace osl {

    /**
     * Reference counter
     */
    class RefCounter {
    private:
        int _count;
    public:
        RefCounter();
	RefCounter(int count);
        virtual ~RefCounter();
	virtual int ref();
	virtual int unref();
	virtual int & ref_count();
    };

    /**
     * Shared reference counter
     */
    class SharedRefCounter : public RefCounter {
    private:
        Mutex _mutex;
    public:
        SharedRefCounter();
	SharedRefCounter(int count);
        virtual ~SharedRefCounter();
	virtual int ref();
	virtual int unref();
    };

    /**
     * auto ref
     */
    template <typename T, typename S>
    class AutoRefBase {
    private:
        Ref<RefCounter> _counter;
        T * _t;
    public:
        explicit AutoRefBase();
	explicit AutoRefBase(T * _t);
	explicit AutoRefBase(T * _t, const Ref<RefCounter> & counter);
        AutoRefBase(const AutoRefBase<T, S> & other);
        virtual ~AutoRefBase();
        AutoRefBase<T, S> & operator= (T * t);
        AutoRefBase<T, S> & operator= (const AutoRefBase<T, S> & other);
        const T & operator* () const;
	T & operator* ();
	T * operator-> () const;
        T * operator& ();
	T * operator& () const;
	bool operator== (const AutoRefBase<T, S> & other) const;
        int ref_count();
	bool nil() const;
	Ref<RefCounter> & counter();
	const Ref<RefCounter> & counter() const;
    private:
        void _retain();
        void _release();
        void _copy(const AutoRefBase<T, S> & other);
    };

    template <typename T, typename S>
    AutoRefBase<T, S>::AutoRefBase() : _t(NULL) {
    }
        
    template <typename T, typename S>
    AutoRefBase<T, S>::AutoRefBase(T * _t) : _t(_t) {
        _retain();
    }

    template <typename T, typename S>
    AutoRefBase<T, S>::AutoRefBase(T * _t, const Ref<RefCounter> & counter) : _counter(counter), _t(_t) {
        _retain();
    }
        
    template <typename T, typename S>
    AutoRefBase<T, S>::AutoRefBase(const AutoRefBase<T, S> & other) : _t(NULL) {
        _copy(other);
    }
        
    template <typename T, typename S>
    AutoRefBase<T, S>::~AutoRefBase() {
        _release();
    }
    
    template <typename T, typename S>
    AutoRefBase<T, S> & AutoRefBase<T, S>::operator= (T * t) {
        _release();
        _counter = NULL;
        _t = t;
        _retain();
        return *this;
    }
	
    template <typename T, typename S>
    AutoRefBase<T, S> & AutoRefBase<T, S>::operator= (const AutoRefBase<T, S> & other) {
        _copy(other);
        return *this;
    }
	
    template <typename T, typename S>
    const T & AutoRefBase<T, S>::operator* () const {
        if (!_t) {
            throw NullException("null exception (operator*)");
        }
        return *_t;
    }

    template <typename T, typename S>
    T & AutoRefBase<T, S>::operator* () {
        if (!_t) {
            throw NullException("null exception (operator*)");
        }
        return *_t;
    }

    template <typename T, typename S>
    T * AutoRefBase<T, S>::operator-> () const {
        if (!_t) {
            throw NullException("null exception (operator->)");
        }
        return _t;
    }
    
    template <typename T, typename S>
    T * AutoRefBase<T, S>::operator& () {
        return _t;
    }

    template <typename T, typename S>
    T * AutoRefBase<T, S>::operator& () const {
        return _t;
    }

    template <typename T, typename S>
    bool AutoRefBase<T, S>::operator== (const AutoRefBase<T, S> & other) const {
	return _t == other._t;
    }
        
    template <typename T, typename S>
    int AutoRefBase<T, S>::ref_count() {
        return _counter.nil() == false ? _counter->ref_count() : -1;
    }

    template <typename T, typename S>
    bool AutoRefBase<T, S>::nil() const {
        return _t == NULL;
    }

    template <typename T, typename S>
    Ref<RefCounter> & AutoRefBase<T, S>::counter() {
	return _counter;
    }

    template <typename T, typename S>
    const Ref<RefCounter> & AutoRefBase<T, S>::counter() const {
	return _counter;
    }
        
    template <typename T, typename S>
    void AutoRefBase<T, S>::_retain() {
        if (_t == NULL) {
	    return;
	}
	if (_counter.nil()) {
	    _counter = Ref<RefCounter>(new S);
	}
	_counter->ref();
    }
        
    template <typename T, typename S>
    void AutoRefBase<T, S>::_release() {
        if (_counter.nil() == false && _counter->unref() == 0) {
            if (_counter.nil() == false) {
		_counter.dealloc();
	    }
	    if (_t) {
		delete _t;
		_t = NULL;
	    }
        }
    }
        
    template <typename T, typename S>
    void AutoRefBase<T, S>::_copy(const AutoRefBase<T, S> & other) {
        if (_t == other._t) {
	    return;
	}

	Ref<RefCounter> x_counter = _counter;
	T * x_t = _t;

	_counter = other._counter;
	_t = other._t;
	_retain();

	//
	if (x_counter.nil() == false && x_counter->unref() == 0) {
            if (x_counter.nil() == false) {
		x_counter.dealloc();
	    }
	    if (x_t) {
		delete x_t;
		x_t = NULL;		// TODO: ?
	    }
        }
    }

    /**
     * not allow void
     */
    template<>
    class AutoRefBase<void, void> {
    private:
    public:
    };
	
    template <typename T>
    class AutoRef : public AutoRefBase<T, SharedRefCounter> {
    public:
	AutoRef() {}
	AutoRef(T * t)
	    : AutoRefBase<T, SharedRefCounter>(t) {}
	virtual ~AutoRef() {}
    };

    template <typename T>
    class UnsafeAutoRef : public AutoRefBase<T, RefCounter> {
    public:
	UnsafeAutoRef() {}
	UnsafeAutoRef(T * t)
	    : AutoRefBase<T, RefCounter>(t) {}
	virtual ~UnsafeAutoRef() {}
    };

    /**
     * not allow void
     */
    template<>
    class AutoRef<void> {
    private:
    public:
    };

    /**
     * not allow void
     */
    template<>
    class UnsafeAutoRef<void> {
    private:
    public:
    };
}

#endif
