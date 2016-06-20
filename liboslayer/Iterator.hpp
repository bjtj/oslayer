#ifndef __ITERATOR_HPP__
#define __ITERATOR_HPP__

#include <string>
#include <vector>

namespace UTIL {
	
	template <typename T>
	class Iterator {
	private:
		size_t _idx;
		std::vector<T> & _vec;
		typename std::vector<T>::iterator _iter;
	private:
		Iterator(const Iterator<T> & other); // not allowed
		Iterator & operator=(const Iterator<T> & other); // not allowed
	public:
		Iterator(std::vector<T> & vec) : _idx(0), _vec(vec), _iter(vec.begin()) {}
		virtual ~Iterator() {}
		bool hasNext() {
			return _iter != _vec.end();
		}
		size_t idx() {
			return _idx;
		}
		T & get() {
			return *_iter;
		}
		T & next() {
			if (!hasNext()) {
				throw "out of bound";
			}
			_idx++;
			return *_iter++;
		}
		T & operator* () {
			return get();
		}
		Iterator<T> & operator++ (int) {
			next();
			return *this;
		}
	};
	
}

#endif
