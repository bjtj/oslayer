#ifndef __PROPERTY_MAP_HPP__
#define __PROPERTY_MAP_HPP__

#include <string>
#include <vector>
#include "StringElements.hpp"
#include "os.hpp"

namespace UTIL {

	/**
	 * @brief
	 */
	class PropertyMap {
	private:
		std::vector<KeyValueAttr> _elements;
	public:
		PropertyMap() {}
		virtual ~PropertyMap() {}
		std::vector<KeyValueAttr> & elements() {
			return _elements;
		}
		size_t size() const {
			return _elements.size();
		}
        void clear() {
            _elements.clear();
        }
		void erase(const std::string & name) {
			for (std::vector<KeyValueAttr>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->key() == name) {
					_elements.erase(iter);
					return;
				}
			}
		}
		bool has(const std::string & name) const {
			for (std::vector<KeyValueAttr>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->key() == name) {
					return true;
				}
			}
			return false;
		}
		KeyValueAttr & get(const std::string & name) {
			for (std::vector<KeyValueAttr>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->key() == name) {
					return *iter;
				}
			}
			_elements.push_back(KeyValueAttr(name));
			return get(name);
		}
		KeyValueAttr get_const(const std::string & name) const {
			for (std::vector<KeyValueAttr>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->key() == name) {
					return *iter;
				}
			}
			throw OS::Exception("no item found");
		}
		void set(const KeyValue & kv) {
			get(kv.key()).value() = kv.value();
		}
		void set(const KeyValueAttr & kva) {
			get(kva.key()).value() = kva.value();
		}
		void append(const PropertyMap & m) {
			for (size_t i = 0; i < m.size(); i++) {
				set(m[i]);
			}
		}
		void append(const std::map<std::string, std::string> & m) {
			for (std::map<std::string, std::string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
				get(iter->first).value() = iter->second;
			}
		}
		std::map<std::string, std::string> toStdMap() {
			std::map<std::string, std::string> ret;
			for (size_t i = 0; i < _elements.size(); i++) {
				ret[_elements[i].key()] = _elements[i].value();
			}
			return ret;
		}
		KeyValueAttr & operator[] (size_t index) {
			return _elements[index];
		}
		KeyValueAttr operator[] (size_t index) const {
			return _elements[index];
		}
		std::string & operator[] (const std::string & name) {
			return get(name).value();
		}
		void operator= (const std::map<std::string, std::string> & m) {
			clear();
			append(m);
		}
	};
}

#endif
