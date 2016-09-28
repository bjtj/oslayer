#ifndef __RANDOM_HPP__
#define __RANDOM_HPP__

#include "os.hpp"

namespace OS {
	
	class Random {
	public:
		Random();
		Random(unsigned seed);
		virtual ~Random();
		void setSeed(unsigned seed);
		int next();
	};
}

#endif
