#include "AutoRef.hpp"

namespace OS {
	
    SharedCounter::SharedCounter() : _count(0), sem(1) {
    }
    SharedCounter::~SharedCounter() {
    }
    void SharedCounter::countUp() {
        sem.wait();
        _count++;
        sem.post();
    }
    bool SharedCounter::countDownAndCheckZero() {
        sem.wait();
        countDown();
        bool z = zero();
        sem.post();
        return z;
    }
    void SharedCounter::countDown() {
        if (_count > 0) {
            _count--;
        }
    }
    bool SharedCounter::zero() {
        return _count == 0;
    }
    int SharedCounter::count() {
        return _count;
    }

}
