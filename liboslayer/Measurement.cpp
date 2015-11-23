#include "Measurement.hpp"

namespace UTIL {


	Measurement::Measurement() {
	}
	Measurement::~Measurement() {
	}

	unsigned long Measurement::pin() {

		unsigned long prev = OS::tick_milli();
		unsigned long curr = prev;

		if (pins.size() > 0) {
			prev = *pins.rbegin();
		}
		
		pins.push_back(curr);

		return curr - prev;
	}
	unsigned long Measurement::collect() {

		unsigned long prev, curr = OS::tick_milli();

		if (pins.size() > 0) {
			prev = *pins.rbegin();
			pins.pop_back();
		}

		return curr - prev;
	}
}