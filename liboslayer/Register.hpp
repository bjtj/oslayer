#ifndef __REGISTER_HPP__
#define __REGISTER_HPP__

#include <string>
#include <vector>

namespace osl {
	
	/**
	 * @brief 
	 */
	template <typename T>
	class Register {
	public:

		/**
		 * @brief tester for query or various traversing
		 */
		class Tester {
		public:
			Tester() {}
			virtual ~Tester() {}
			virtual bool test(T & t) const = 0;
		};
	private:

		/**
		 * @brief node (id, data)
		 */
		class Node {
		private:
			size_t _id;
			T _data;
		public:
			Node(size_t id, const T & data) : _id(id), _data(data) {}
			virtual ~Node() {}
			size_t id() { return _id; }
			T & data() { return _data; }
		};
	private:
		size_t _seed;
		std::vector<Node> lst;
	public:
		Register() : _seed(0) {}
		virtual ~Register() {}

		void clear() {
			lst.clear();
			_seed = 0;
		}
		size_t size() {
			return lst.size();
		}
		size_t & seed() {
			return _seed;
		}
		size_t next_seed() {
			return _seed++;
		}
		size_t reg(const T & t) {
			size_t id = next_seed();
			lst.push_back(Node(id, t));
			return id;
		}
		void unreg(size_t id) {
			for (size_t i = 0; i < lst.size(); i++) {
				if (lst[i].id() == id) {
					lst.erase(lst.begin() + i);
					return;
				}
			}
		}
		std::vector<T> list() {
			std::vector<T> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				ret.push_back(lst[i].data());
			}
			return ret;
		}
		std::vector<T> query(const Tester & tester) {
			std::vector<T> ret;
			for (size_t i = 0; i < lst.size(); i++) {
				if (tester.test(lst[i].data())) {
					ret.push_back(lst[i].data());
				}
			}
			return ret;
		}
		bool contains(const T & t) {
			for (size_t i = 0; i < lst.size(); i++) {
				if (lst[i].data() == t) {
					return true;
				}
			}
			return false;
		}
		bool containsById(size_t id) {
			for (size_t i = 0; i < lst.size(); i++) {
				if (lst[i].id() == id) {
					return true;
				}
			}
			return false;
		}
		T & operator[] (size_t idx) {
			return lst[idx].data();
		}
	};
}

#endif
