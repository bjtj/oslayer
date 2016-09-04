#ifndef __CONDITION_HPP__
#define __CONDITION_HPP__

namespace UTIL {

	class Condition {
	public:
		Condition() {}
		virtual ~Condition() {}
		virtual bool test(void * t) const = 0;
	};
}

#endif
