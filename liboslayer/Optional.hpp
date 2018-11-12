#ifndef __OPTIONAL_HPP__
#define __OPTIONAL_HPP__

#include "os.hpp"

namespace osl {

    template <typename T>
    class Optional
    {
    private:
	T * _t;
    public:
	Optional() : _t(NULL) {
	}
	Optional(T & t) : _t(&t) {
	}
	virtual ~Optional() {
	}
	T & operator*() {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return *_t;
	}
	T * operator->() const {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return _t;
	}
	T * operator& () {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return _t;
	}
	T * operator& () const {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return _t;
	}
	bool operator== (const Optional<T> & other) const {
	    return _t == other._t;
	}
	bool nil() const {
	    return _t == NULL;
	}
    };

    template <>
    class Optional<void> {
    private:
    public:
    };
}

#endif
