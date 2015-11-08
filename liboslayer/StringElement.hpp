#ifndef __STRING_ELEMENT_HPP__
#define __STRING_ELEMENT_HPP__

#include <string>
#include <map>
#include <vector>
#include "os.hpp"

namespace UTIL {

	/**
	 * @brief name value
	 */
	class NameValue {
	private:
		std::string name;
		std::string value;

	public:
		NameValue() {}
		NameValue(const std::string & name) : name(name) {}
		NameValue(const std::string & name, const std::string & value) : name(name), value(value) {}
		virtual ~NameValue() {}
		void setName(const std::string & name) {this->name = name;}
		void setValue(const std::string & value) {this->value = value;}
		std::string & getName() {return name;}
		std::string & getValue() {return value;}
		const std::string & getName() const {return name;}
		const std::string & getValue() const {return value;}
		bool operator==(const std::string & name) const {
			return (!name.compare(name) ? true : false);
		}
	};

	/**
	 * @brief string map
	 */
	class StringMap : public std::map<std::string, std::string> {
    private:
	public:
		StringMap() {}
		virtual ~StringMap() {}
        
        std::string & operator[] (const std::string & name) {
            return std::map<std::string, std::string>::operator[](name);
        }
        
        std::string get(const std::string & name) const {
            for (StringMap::const_iterator iter = begin(); iter != end(); iter++) {
                if (iter->first == name) {
                    return iter->second;
                }
            }
            return "";
        }
//
//        const std::string & operator[] (const std::string & name) const {
//            
//            for (StringMap::const_iterator iter = begin(); iter != end(); iter++) {
//                if (iter->first == name) {
//                    return iter->second;
//                }
//            }
//            throw OS::Exception("no element found", -1, 0);
//        }
	};

	/**
	 * @brief linked string map
	 */
	class LinkedStringMap {
    public:
        class iterator {
            const LinkedStringMap * container;
            
        };
	private:
		std::vector<NameValue> elements;
	private:
		NameValue & get(const std::string & name) {
			for (size_t i = 0; i < elements.size(); i++) {
				NameValue & nv = elements[i];
				if (nv == name) {
					return nv;
				}
			}
			elements.push_back(NameValue(name));
			return get(name);
		}
		const NameValue & get(const std::string & name) const {
			for (size_t i = 0; i < elements.size(); i++) {
				const NameValue & nv = elements[i];
				if (nv == name) {
					return nv;
				}
			}
            static const NameValue empty;
			return empty;
		}
	public:
		LinkedStringMap() {
		}

		virtual ~LinkedStringMap() {
		}

		size_t size() const {
			return elements.size();
		}
        
        void clear() {
            elements.clear();
        }

		const std::string & operator[] (const std::string & name) const {
			return get(name).getValue();
		}
		std::string & operator[] (const std::string & name) {
			return get(name).getValue();
		}
		const NameValue & operator[] (size_t index) const {
			return elements[index];
		}
		NameValue & operator[] (size_t index) {
			return elements[index];
		}
	};
}

#endif