#ifndef __AUTO_RELEASE_HPP__
#define __AUTO_RELEASE_HPP__

namespace UTIL {

	template<typename T>
	class AutoRelease {
	private:
		T * target;
	public:
		AutoRelease(T * target) : target(target) {
		}
		virtual ~AutoRelease() {
			release(target);
		}
		virtual void release(T * target) {
			delete target;
		}

		T & operator*() {
			return *target;
		}
		T * operator->() {
			return target;
		}
		T * operator&() {
			return target;
		}
	};

}

#endif