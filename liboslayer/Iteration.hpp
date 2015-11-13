#ifndef __ITERATION_HPP__
#define __ITERATION_HPP__

namespace UTIL {

	/**
     * @brief iterator callback
     */
	template <typename T>
	class IteratorHandler {
	private:
	public:
		IteratorHandler() {}
		virtual ~IteratorHandler() {}
		virtual void onItem(T & item) = 0;
		virtual bool wantFinish() {return false;}
	};
}

#endif