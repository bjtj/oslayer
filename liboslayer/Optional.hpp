#ifndef __OPTIONAL_HPP__
#define __OPTIONAL_HPP__

#include "os.hpp"
#include <memory>

namespace osl {

    template <typename T>
    class Optional
    {
    private:
	T * _t;	
    public:
	Optional() : _t(NULL) {
	}
	Optional(const T & t) : _t(NULL) {
	    std::allocator<T> al;
	    _t = al.allocate(1);
	    al.construct(_t, t);
	}
	Optional(const Optional<T> & other) : _t(NULL) {
	    if (other._t) {
		std::allocator<T> al;
		_t = al.allocate(1);
		al.construct(_t, *(other._t));
	    }
	}
	virtual ~Optional() {
	    if (_t) {
		std::allocator<T> al;
		al.destroy(_t);
		al.deallocate(_t, 1);
	    }
	}
	T & operator*() {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return _t[0];
	}
	T * operator->() const {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return &(_t[0]);
	}
	T * operator& () {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return &(_t[0]);
	}
	T * operator& () const {
	    if (nil()) {
		throw NullException("null exception");
	    }
	    return &(_t[0]);
	}
	Optional<T> operator= (const Optional<T> & other) {
	    if (_t) {
		std::allocator<T> al;
		al.destroy(_t);
		al.deallocate(_t, 1);
	    }

	    if (other._t) {
		std::allocator<T> al;
		_t = al.allocate(1);
		al.construct(_t, *(other._t));
	    } else {
		_t = NULL;
	    }
	    return *this;
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
