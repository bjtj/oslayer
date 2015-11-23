#ifndef __MEASUREMENT_HPP__
#define __MEASUREMENT_HPP__

#include "os.hpp"
#include <vector>

namespace UTIL {

	class Measurement {
	private:

		std::vector<unsigned long> pins;

	public:
		Measurement();
		virtual ~Measurement();

		unsigned long pin();
		unsigned long collect();
	};
}

#endif