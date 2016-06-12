#include "Random.hpp"

namespace OS {
	
	Random::Random() {
	}
	Random::Random(unsigned long seed) {
		srand(seed);
	}
	Random::~Random() {
	}

	bool Random::nextBool() {
		return (bool)(rand() % 2);
	}
	int Random::nextInteger() {
		return (int)rand();
	}
	long Random::nextLong() {
		return (long)rand();
	}
	void Random::setSeed(unsigned long seed) {
		srand(seed);
	}
}
