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
		std::string _name;
		std::string _value;

	public:
		NameValue() {}
		NameValue(const std::string & name) : _name(name) {}
		NameValue(const std::string & name, const std::string & value) : _name(name), _value(value) {}
		virtual ~NameValue() {}
		void setName(const std::string & name) {this->_name = name;}
		void setValue(const std::string & value) {this->_value = value;}
		std::string & getName() {return _name;}
		std::string & getValue() {return _value;}
		const std::string & getName() const {return _name;}
		const std::string & getValue() const {return _value;}
		std::string & name() { return _name; }
		std::string & value() { return _value; }
		bool operator==(const std::string & name) const {
			return (!this->_name.compare(name) ? true : false);
		}
	};
    
    /**
     * @brief name value list
     */
    class NameValueList : public std::vector<NameValue> {
    private:
    public:
        NameValueList() {}
		NameValueList(const std::vector<NameValue> & lst) {
			this->clear();
			this->insert(this->end(), lst.begin(), lst.end());
		}
        virtual ~NameValueList() {}
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
	};

	/**
	 * @brief linked string map
	 */
	class LinkedStringMap {

	private:
		std::vector<NameValue> _elements;

	public:
		LinkedStringMap() {
		}

		virtual ~LinkedStringMap() {
		}

		std::vector<NameValue> elements() {
			return _elements;
		}

		std::vector<NameValue> toNameValueList() const {
			return NameValueList(_elements);
		}

		size_t size() const {
			return _elements.size();
		}
        
        void clear() {
            _elements.clear();
        }

		NameValue & get(const std::string & name) {
			for (size_t i = 0; i < _elements.size(); i++) {
				NameValue & nv = _elements[i];
				if (nv == name) {
					return nv;
				}
			}
			_elements.push_back(NameValue(name));
			return get(name);
		}
		NameValue const_get(const std::string & name) const {
			for (size_t i = 0; i < _elements.size(); i++) {
				const NameValue & nv = _elements[i];
				if (nv == name) {
					return nv;
				}
			}
			return NameValue();
		}
		NameValue & getByIndex(size_t index) {
			return _elements[index];
		}
		const NameValue & const_getByIndex(size_t index) const {
			return _elements[index];
		}

		std::string & operator[] (const std::string & name) {
			return get(name).getValue();
		}
		NameValue & operator[] (size_t index) {
			return _elements[index];
		}
	};


	/**
	 * @brief NameProperty
	 */
	class NameProperty {
	private:
		std::string _name;
		std::string _value;
		LinkedStringMap properties;

	public:
		NameProperty() {}
		NameProperty(const std::string & name) : _name(name) {}
		NameProperty(const std::string & name, const std::string & value) : _name(name), _value(value) {}
		virtual ~NameProperty() {}
		void setName(const std::string & name) {this->_name = name;}
		void setValue(const std::string & value) {this->_value = value;}
		std::string & name() {return _name;}
		std::string & value() {return _value;}
		std::string & getName() {return _name;}
		std::string & getValue() {return _value;}
		const std::string & getName() const {return _name;}
		const std::string & getValue() const {return _value;}
		LinkedStringMap & getProperties() {return properties;}
		const LinkedStringMap & getProperties() const {return properties;}
		std::string & getProperty(const std::string & name) {return properties[name];}
		std::string getProperty(const std::string & name) const {return properties.const_get(name).getValue();}
		void setProperty(const std::string & name, const std::string & value) {properties[name] = value;}
		std::string & operator[] (const std::string & name) {return properties[name];}
		bool operator==(const std::string & name) const {
			return (!this->_name.compare(name) ? true : false);
		}
		void operator=(const std::string & value) {
			this->_value = value;
		}
	};


	/**
	 * @brief linked string map
	 */
	class LinkedStringProperties {
	private:
		std::vector<NameProperty> elements;
	public:
		LinkedStringProperties() {
		}
		virtual ~LinkedStringProperties() {
		}
		size_t size() const {
			return elements.size();
		}
        void clear() {
            elements.clear();
        }
		NameProperty & get(const std::string & name) {
			for (size_t i = 0; i < elements.size(); i++) {
				NameProperty & np = elements[i];
				if (np == name) {
					return np;
				}
			}
			elements.push_back(NameProperty(name));
			return get(name);
		}
		const NameProperty & const_get(const std::string & name) const {
			for (size_t i = 0; i < elements.size(); i++) {
				const NameProperty & np = elements[i];
				if (np == name) {
					return np;
				}
			}
			throw OS::Exception("no item found", -1, 0);
		}
		bool has(const std::string & name) const {
			for (size_t i = 0; i < elements.size(); i++) {
				const NameProperty & np = elements[i];
				if (np == name) {
					return true;
				}
			}
			return false;
		}
		NameProperty & operator[] (const std::string & name) {
			return get(name);
		}
		const NameProperty & operator[] (size_t index) const {
			return elements[index];
		}
		NameProperty & operator[] (size_t index) {
			return elements[index];
		}
	};
    
    /**
     * @brief string list map
     */
    class StringListMap : public std::map<std::string, std::vector<std::string> > {
    private:
    public:
        StringListMap() {
        }
        
        virtual ~StringListMap() {
        }
        
        void append(const std::string & name, const std::string & value) {
            this->operator[](name).push_back(value);
        }
        
        std::string getFirstValue(const std::string & name) {
            return this->operator[](name)[0];
        }
        
        NameValueList toNameValueList() const {
            NameValueList lst;
            
            for (StringListMap::const_iterator iter = begin(); iter != end(); iter++) {
                std::string name = iter->first;
                const std::vector<std::string> & values = iter->second;
                
                for (size_t i = 0; i < values.size(); i++) {
                    lst.push_back(NameValue(name, values[i]));
                }
            }
            return lst;
        }
    };
}

#endif
