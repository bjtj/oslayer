#ifndef __LIFETIME_HPP__
#define __LIFETIME_HPP__

#include "os.hpp"

namespace osl {

    /**
     * @brief lifetime
     */
    class Lifetime {
    private:
	unsigned long _startTick;
    public:
	Lifetime() : _startTick(0) {
	    resetLifetime();
	}
	virtual ~Lifetime() {/**/}
	unsigned long & startTick() {
	    return _startTick;
	}
	unsigned long lifetime() {
	    return tick_milli() - _startTick;;
	}
	void resetLifetime() {
	    _startTick = tick_milli();
	}
    };
}

#endif
