#ifndef __ITERATOR_HPP__
#define __ITERATOR_HPP__

#include "os.hpp"
#include <string>
#include <vector>
#include "Condition.hpp"

namespace UTIL {

	/**
	 * 
	 */
	template <typename T>
	class Iterator {
	private:
		size_t _idx;
		std::vector<T> & _vec;
		typename std::vector<T>::iterator _iter;
	private:
	public:
		Iterator(std::vector<T> & vec) : _idx(0), _vec(vec), _iter(vec.begin()) {
		}
		Iterator(std::vector<T> & vec, typename std::vector<T>::iterator iter) : _idx(0), _vec(vec), _iter(iter) {
		}
		virtual ~Iterator() {
		}
		void rewind() {
			_idx = 0;
			_iter = _vec.begin();
		}
		bool has() {
			return (_iter != _vec.end());
		}
		bool has_next() {
			return (_iter + 1) != _vec.end();
		}
		T & next() {
			if (!has()) {
				throw OS::Exception("out of bound");
			}
			_idx++;
			return *(_iter++);
		}
		size_t idx() {
			return _idx;
		}
		typename std::vector<T>::iterator begin() {
			return _vec.begin();
		}
		typename std::vector<T>::iterator iter() {
			return _iter;
		}
		typename std::vector<T>::iterator end() {
			return _vec.end();
		}
		std::vector<T> collect(Condition * condition) {
			std::vector<T> lst;
			while (has()) {
				T & t = next();
				if (condition == NULL || condition->test(&t)) {
					lst.push_back(t);
				}
			}
			return lst;
		}
		T & operator* () {
			return *_iter;
		}
		T * operator& () {
			return &(*_iter);
		}
		T * operator-> () {
			return &(*_iter);
		}
		Iterator<T> & operator++ () {
			++_idx;
			++_iter;
			return *this;
		}
		Iterator<T> operator++ (int) {
			Iterator ret(*this);
			_idx++;
			_iter++;
			return ret;
		}
		Iterator<T> & operator-- () {
			--_idx;
			--_iter;
			return *this;
		}
		Iterator<T> operator-- (int) {
			Iterator ret(*this);
			_idx--;
			_iter--;
			return ret;
		}
		Iterator<T> operator+ (const size_t & i) const {
			Iterator ret(*this);
			ret += i;
			return ret;
		}
		Iterator<T> operator- (const size_t & i) const {
			Iterator ret(*this);
			ret -= i;
			return ret;
		}
		Iterator<T> & operator+= (const size_t & i) const {
			_idx += i;
			_iter += i;
			return *this;
		}
		Iterator<T> & operator-= (const size_t & i) const {
			_idx -= i;
			_iter -= i;
			return *this;
		}
	};


	template <typename T>
	class ConstIterator {
	private:
		size_t _idx;
		const std::vector<T> & _vec;
		typename std::vector<T>::const_iterator _iter;
	private:
	public:
		ConstIterator(const std::vector<T> & vec) : _idx(0), _vec(vec), _iter(vec.begin()) {
		}
		ConstIterator(const std::vector<T> & vec, typename std::vector<T>::const_iterator iter) : _idx(0), _vec(vec), _iter(iter) {
		}
		virtual ~ConstIterator() {
		}
		void rewind() {
			_idx = 0;
			_iter = _vec.begin();
		}
		bool has() {
			return (_iter != _vec.end());
		}
		bool has_next() {
			return (_iter + 1) != _vec.end();
		}
		T & next() {
			if (!has()) {
				throw OS::Exception("out of bound");
			}
			_idx++;
			return *(_iter++);
		}
		size_t idx() {
			return _idx;
		}
		typename std::vector<T>::const_iterator begin() {
			return _vec.begin();
		}
		typename std::vector<T>::const_iterator iter() {
			return _iter;
		}
		typename std::vector<T>::const_iterator end() {
			return _vec.end();
		}
		std::vector<T> collect(Condition * condition) {
			std::vector<T> lst;
			while (has()) {
				T & t = next();
				if (condition == NULL || condition->test(&t)) {
					lst.push_back(t);
				}
			}
			return lst;
		}
		T operator* () const {
			return *_iter;
		}
		T * operator& () {
			return &(*_iter);
		}
		T * operator-> () {
			return &(*_iter);
		}
		ConstIterator<T> & operator++ () {
			++_idx;
			++_iter;
			return *this;
		}
		ConstIterator<T> operator++ (int) {
			ConstIterator ret(*this);
			_idx++;
			_iter++;
			return ret;
		}
		ConstIterator<T> & operator-- () {
			--_idx;
			--_iter;
			return *this;
		}
		ConstIterator<T> operator-- (int) {
			ConstIterator ret(*this);
			_idx--;
			_iter--;
			return ret;
		}
		ConstIterator<T> operator+ (const size_t & i) const {
			ConstIterator ret(*this);
			ret += i;
			return ret;
		}
		ConstIterator<T> operator- (const size_t & i) const {
			ConstIterator ret(*this);
			ret -= i;
			return ret;
		}
		ConstIterator<T> & operator+= (const size_t & i) const {
			_idx += i;
			_iter += i;
			return *this;
		}
		ConstIterator<T> & operator-= (const size_t & i) const {
			_idx -= i;
			_iter -= i;
			return *this;
		}
	};
}

#endif
