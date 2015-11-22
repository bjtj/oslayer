#include "AutoRef.hpp"

namespace UTIL {
    SharedCounter::SharedCounter() : _count(0) {
    }
    SharedCounter::~SharedCounter() {
    }
    void SharedCounter::countUp() {
        _count++;
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