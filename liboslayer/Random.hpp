#ifndef __RANDOM_HPP__
#define __RANDOM_HPP__

#include "os.hpp"

namespace OS {
	
	class Random {
	public:
		Random();
		Random(unsigned long seed);
		virtual ~Random();

		bool nextBool();
		int nextInteger();
		long nextLong();
		void setSeed(unsigned long seed);
	};
}

#endif
