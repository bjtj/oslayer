#ifndef __CONDITION_HPP__
#define __CONDITION_HPP__

namespace osl {

    class Condition {
    public:
	Condition() {}
	virtual ~Condition() {}
	virtual bool test(void * t) const = 0;
    };
}

#endif
