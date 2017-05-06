#ifndef __SEMAPHORE_HPP__
#define __SEMAPHORE_HPP__

#include "os.hpp"

namespace OS {

	/**
	 * @brief semaphore
	 */
	class Semaphore {
	private:
		int initial;
		mutable SEM_HANDLE handle;
	public:
		Semaphore(int initial);
		Semaphore(const Semaphore & other);
		virtual ~Semaphore();
		void wait() const;
		void post() const;
	};
}

#endif
