#ifndef __ITERATOR_HPP__
#define __ITERATOR_HPP__

#include "os.hpp"
#include <string>
#include <vector>
#include "Condition.hpp"

namespace UTIL {
	
	template <typename T>
	class Iterator {
	private:
		size_t _idx;
		std::vector<T> & _vec;
		typename std::vector<T>::iterator _iter;
	private:
		// Iterator(const Iterator<T> & other); // not allowed
		// Iterator & operator=(const Iterator<T> & other); // not allowed
	public:
		Iterator(std::vector<T> & vec) : _idx(0), _vec(vec), _iter(vec.begin()) {}
		virtual ~Iterator() {}
		void reset() {
			_idx = 0;
			_iter = _vec.begin();
		}
		bool hasNext() {
			return _iter != _vec.end();
		}
		size_t idx() {
			return _idx;
		}
		typename std::vector<T>::iterator iter() {
			return _iter;
		}
		T & get() {
			return *_iter;
		}
		T & next() {
			if (!hasNext()) {
				throw OS::Exception("out of bound");
			}
			_idx++;
			return *_iter++;
		}
		std::vector<T> list(const Condition & condition) {
			std::vector<T> lst;
			for (;hasNext(); next()) {
				if (condition.test(&get())) {
					lst.push_back(get());
				}
			}
			return lst;
		}
		T & operator* () {
			return get();
		}
		T * operator& () {
			return &get();
		}
		T * operator-> () {
			return &get();
		}
		Iterator<T> & operator++ (int) {
			next();
			return *this;
		}
	};
}

#endif
