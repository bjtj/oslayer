#include "CountDownLatch.hpp"

namespace osl {

	using namespace std;
	
	
	CountDownLatch::CountDownLatch(size_t count) : count(count), sem(1) {
	}
	CountDownLatch::~CountDownLatch() {
	}
	void CountDownLatch::await() {
		bool done = false;
		while (!done) {
			sem.wait();
			if (count == 0) {
				done = true;
			}
			sem.post();
		}
	}
	void CountDownLatch::await(unsigned long timeout) {
		bool done = false;
		unsigned long tick = tick_milli();
		while (!done) {

			if (tick_milli() - tick >= timeout) {
				throw Exception("timeout occurred");
			}
			
			sem.wait();
			if (count == 0) {
				done = true;
			}
			sem.post();
		}
	}
	void CountDownLatch::countDown() {
		sem.wait();
		if (count > 0) {
			count--;
		}
		sem.post();
	}
	size_t CountDownLatch::getCount() {
		return count;
	}
}
