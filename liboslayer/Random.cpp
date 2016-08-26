#include "Random.hpp"

namespace OS {
	
	Random::Random() {
	}
	Random::Random(unsigned long seed) {
		srand(seed);
	}
	Random::~Random() {
	}
	void Random::setSeed(unsigned long seed) {
		srand(seed);
	}
	int Random::next() {
		return rand();
	}
}
