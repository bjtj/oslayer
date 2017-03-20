#ifndef __COUNT_DOWN_LATCH_HPP__
#define __COUNT_DOWN_LATCH_HPP__

#include "os.hpp"

namespace UTIL {
	
	class CountDownLatch {
	private:
		size_t count;
		OS::Semaphore sem;
	public:
		CountDownLatch(size_t count);
		virtual ~CountDownLatch();
		void await();
		void await(unsigned long timeout);
		void countDown();
		size_t getCount();
	};
}

#endif
