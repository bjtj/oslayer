#include "Random.hpp"

namespace OS {
	
	Random::Random() {
	}
	Random::Random(unsigned seed) {
        setSeed(seed);
	}
	Random::~Random() {
	}
	void Random::setSeed(unsigned seed) {
		srand(seed);
	}
	int Random::next() {
		return rand();
	}
}
