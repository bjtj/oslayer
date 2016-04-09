#ifndef __PROPERTY_MAP_HPP__
#define __PROPERTY_MAP_HPP__

#include <string>
#include <vector>
#include "StringElement.hpp"

namespace UTIL {

	class NameValueAttributes : public NameValue {
	private:
		LinkedStringMap _attributes;
	public:
		NameValueAttributes() {}
		NameValueAttributes(const std::string & name) : NameValue(name) {}
		virtual ~NameValueAttributes() {}
		LinkedStringMap & attributes() {
			return _attributes;
		}
		std::string & attr(const std::string & name) {
			return _attributes[name];
		}
	};

	/**
	 * @brief
	 */
	class PropertyMap {
	private:
		std::vector<NameValueAttributes> _elements;
	public:
		PropertyMap() {}
		virtual ~PropertyMap() {}
		std::vector<NameValueAttributes> & elements() {
			return _elements;
		}
		size_t size() const {
			return _elements.size();
		}
        void clear() {
            _elements.clear();
        }
		void erase(const std::string & name) {
			for (std::vector<NameValueAttributes>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->name() == name) {
					_elements.erase(iter);
					return;
				}
			}
		}
		bool has(const std::string & name) const {
			for (std::vector<NameValueAttributes>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->name_const() == name) {
					return true;
				}
			}
			return false;
		}
		NameValueAttributes & get(const std::string & name) {

			for (std::vector<NameValueAttributes>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->name() == name) {
					return *iter;
				}
			}
			_elements.push_back(NameValueAttributes(name));
			return get(name);
		}
		NameValueAttributes get_const(const std::string & name) const {
			for (std::vector<NameValueAttributes>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
				if (iter->name_const() == name) {
					return *iter;
				}
			}
			throw OS::Exception("no item found");
		}
		void set(const NameValue & nv) {
			get(nv.name_const()).value() = nv.value_const();
		}
		void set(const NameValueAttributes & nv) {
			get(nv.name_const()).value() = nv.value_const();
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
				ret[_elements[i].name()] = _elements[i].value();
			}
			return ret;
		}
		NameValueAttributes & operator[] (size_t index) {
			return _elements[index];
		}
		NameValueAttributes operator[] (size_t index) const {
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
