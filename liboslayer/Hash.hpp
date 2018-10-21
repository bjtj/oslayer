#ifndef __HASH_HPP__
#define __HASH_HPP__

namespace osl {

    /**
     * @brief 
     */
    class Hash {
    private:
    public:
	Hash() {}
	virtual ~Hash() {}
	virtual unsigned long hash(const char * str) = 0;
    };

    /**
     * @brief 
     */
    class SimpleHash : public Hash {
    private:
	unsigned long seed;
    public:
	SimpleHash() : seed(0) {}
	SimpleHash(unsigned long seed) : seed(seed) {}
	virtual ~SimpleHash() {}

	void setSeed(unsigned long seed) {
	    this->seed = seed;
	}

	// http://stackoverflow.com/a/7666577
	virtual unsigned long hash(const char * str) {
	    unsigned long hash = seed;
	    int c;
	    while ((c = *str++)) {
		hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
	    }

	    return hash;
	}
    };
	
}

#endif
