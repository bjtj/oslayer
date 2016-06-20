#include "StringElements.hpp"
#include "os.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;
	
	NameValue::NameValue() {}
	NameValue::NameValue(const string & name) : _name(name) {}
	NameValue::NameValue(const string & name, const string & value) : _name(name), _value(value) {}
	NameValue::~NameValue() {}
	string & NameValue::name() {return _name;}
	string & NameValue::value() {return _value;}
	const string NameValue::const_name() const {return _name;}
	const string NameValue::const_value() const {return _value;}
	bool NameValue::operator==(const string & name) const {
		return (!this->_name.compare(name) ? true : false);
	}
    

	NameValueList::NameValueList() {}
	NameValueList::NameValueList(const vector<NameValue> & lst) {
		this->clear();
		this->insert(this->end(), lst.begin(), lst.end());
	}
	NameValueList::~NameValueList() {}



	NamedStringList::NamedStringList() {}
	NamedStringList::NamedStringList(const string & name) : _name(name) {}
	NamedStringList::NamedStringList(const string & name, const string & str) : _name(name) {
		_strs.push_back(str);
	}
	NamedStringList::NamedStringList(const string & name, const vector<string> & strs) : _name(name) {
		_strs = strs;
	}
	NamedStringList::~NamedStringList() {}

	const string NamedStringList::const_name() const {
		return _name;
	}

	string & NamedStringList::name() {
		return _name;
	}

	vector<string> NamedStringList::vec_strs() {
		return _strs;
	}

	string & NamedStringList::first() {
		return *(_strs.begin());
	}

	const string NamedStringList::const_first() const {
		return *(_strs.begin());
	}

	string & NamedStringList::last() {
		return *(_strs.rbegin());
	}

	const string NamedStringList::const_last() const {
		return *(_strs.rbegin());
	}

	const string NamedStringList::first_safe(const string & def) const {
		if (_strs.size() == 0) {
			return def;
		}
		return const_first();
	}

	const string NamedStringList::last_safe(const string & def) const {
		if (_strs.size() == 0) {
			return def;
		}
		return const_last();
	}

	size_t NamedStringList::size() {
		return _strs.size();
	}

	void NamedStringList::clear() {
		_strs.clear();
	}

	string & NamedStringList::operator[] (size_t idx) {
		return _strs[idx];
	}

	void NamedStringList::operator= (const string & str) {
		_strs.clear();
		_strs.push_back(str);
	}

	void NamedStringList::operator= (const char * str) {
		_strs.clear();
		_strs.push_back(str);
	}

	void NamedStringList::operator= (const vector<string> & strs) {
		_strs = strs;
	}

	NamedStringList & NamedStringList::operator+= (const string & str) {
		_strs.push_back(str);
		return *this;
	}

	NamedStringList & NamedStringList::operator+= (const char * str) {
		_strs.push_back(str);
		return *this;
	}

	NamedStringList & NamedStringList::operator+= (const vector<string> & strs) {
		_strs.insert(_strs.end(), strs.begin(), strs.end());
		return *this;
	}


	StringMap::StringMap() {}
	StringMap::~StringMap() {}
        
	string & StringMap::operator[] (const string & name) {
		return map<string, string>::operator[](name);
	}
        
	string StringMap::get(const string & name) const {
		for (StringMap::const_iterator iter = begin(); iter != end(); iter++) {
			if (iter->first == name) {
				return iter->second;
			}
		}
		return "";
	}


	LinkedStringMap::LinkedStringMap() {}
	LinkedStringMap::~LinkedStringMap() {}
	vector<NameValue> LinkedStringMap::elements() {
		return _elements;
	}
	vector<NameValue> LinkedStringMap::toNameValueList() const {
		return NameValueList(_elements);
	}
	size_t LinkedStringMap::size() const {
		return _elements.size();
	}
	void LinkedStringMap::clear() {
		_elements.clear();
	}
	void LinkedStringMap::erase(const string & name) {
		for (vector<NameValue>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->name() == name) {
				_elements.erase(iter);
				return;
			}
		}
	}
	bool LinkedStringMap::contains(const string & name) const {
		for (vector<NameValue>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->const_name() == name) {
				return true;
			}
		}
		return false;
	}
	NameValue & LinkedStringMap::get(const string & name) {
		for (size_t i = 0; i < _elements.size(); i++) {
			NameValue & nv = _elements[i];
			if (nv == name) {
				return nv;
			}
		}
		_elements.push_back(NameValue(name));
		return get(name);
	}
	NameValue LinkedStringMap::const_get(const string & name) const {
		for (size_t i = 0; i < _elements.size(); i++) {
			const NameValue & nv = _elements[i];
			if (nv == name) {
				return nv;
			}
		}
		return NameValue();
	}
	void LinkedStringMap::set(const NameValue & nv) {
		get(nv.const_name()).value() = nv.const_value();
	}
	void LinkedStringMap::append(const LinkedStringMap & m) {
		for (size_t i = 0; i < m.size(); i++) {		
			set(m[i]);
		}
	}
	void LinkedStringMap::append(const map<string, string> & m) {
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			get(iter->first).value() = iter->second;
		}
	}
	map<string, string> LinkedStringMap::toStdMap() {
		map<string, string> ret;
		for (size_t i = 0; i < _elements.size(); i++) {
			ret[_elements[i].name()] = _elements[i].value();
		}
		return ret;
	}
	NameValue & LinkedStringMap::operator[] (size_t index) {
		return _elements[index];
	}
	NameValue LinkedStringMap::operator[] (size_t index) const {
		return _elements[index];
	}
	string & LinkedStringMap::operator[] (const string & name) {
		return get(name).value();
	}
	void LinkedStringMap::operator= (const map<string, string> & m) {
		clear();
		append(m);
	}


	NameProperty::NameProperty() {}
	NameProperty::NameProperty(const string & name) : _name(name) {}
	NameProperty::NameProperty(const string & name, const string & value) : _name(name), _value(value) {}
	NameProperty::~NameProperty() {}
	void NameProperty::setName(const string & name) {this->_name = name;}
	void NameProperty::setValue(const string & value) {this->_value = value;}
	string & NameProperty::name() {return _name;}
	string & NameProperty::value() {return _value;}
	string & NameProperty::getName() {return _name;}
	string & NameProperty::getValue() {return _value;}
	const string & NameProperty::getName() const {return _name;}
	const string & NameProperty::getValue() const {return _value;}
	LinkedStringMap & NameProperty::getProperties() {return properties;}
	const LinkedStringMap & NameProperty::getProperties() const {return properties;}
	string & NameProperty::getProperty(const string & name) {return properties[name];}
	string NameProperty::getProperty(const string & name) const {return properties.const_get(name).value();}
	void NameProperty::setProperty(const string & name, const string & value) {properties[name] = value;}
	string & NameProperty::operator[] (const string & name) {return properties[name];}
	bool NameProperty::operator==(const string & name) const {
		return (!this->_name.compare(name) ? true : false);
	}
	void NameProperty::operator=(const string & value) {
		this->_value = value;
	}



	LinkedStringProperties::LinkedStringProperties() {
	}
	LinkedStringProperties::~LinkedStringProperties() {
	}
	size_t LinkedStringProperties::size() const {
		return elements.size();
	}
	void LinkedStringProperties::clear() {
		elements.clear();
	}
	NameProperty & LinkedStringProperties::get(const string & name) {
		for (size_t i = 0; i < elements.size(); i++) {
			NameProperty & np = elements[i];
			if (np == name) {
				return np;
			}
		}
		elements.push_back(NameProperty(name));
		return get(name);
	}
	const NameProperty & LinkedStringProperties::const_get(const string & name) const {
		for (size_t i = 0; i < elements.size(); i++) {
			const NameProperty & np = elements[i];
			if (np == name) {
				return np;
			}
		}
		throw Exception("no item found", -1, 0);
	}
	bool LinkedStringProperties::contains(const string & name) const {
		for (size_t i = 0; i < elements.size(); i++) {
			const NameProperty & np = elements[i];
			if (np == name) {
				return true;
			}
		}
		return false;
	}
	NameProperty & LinkedStringProperties::operator[] (const string & name) {
		return get(name);
	}
	const NameProperty & LinkedStringProperties::operator[] (size_t index) const {
		return elements[index];
	}
	NameProperty & LinkedStringProperties::operator[] (size_t index) {
		return elements[index];
	}

    

	StringListMap::StringListMap() {
	}
        
	StringListMap::~StringListMap() {
	}
        
	void StringListMap::append(const string & name, const string & value) {
		this->operator[](name).push_back(value);
	}
        
	string StringListMap::getFirstValue(const string & name) {
		return this->operator[](name)[0];
	}
        
	NameValueList StringListMap::toNameValueList() const {
		NameValueList lst;
            
		for (StringListMap::const_iterator iter = begin(); iter != end(); iter++) {
			string name = iter->first;
			const vector<string> & values = iter->second;
                
			for (size_t i = 0; i < values.size(); i++) {
				lst.push_back(NameValue(name, values[i]));
			}
		}
		return lst;
	}



	LinkedStringListMap::LinkedStringListMap() {}
	LinkedStringListMap::~LinkedStringListMap() {}

	void LinkedStringListMap::clear() {
		_elems.clear();
	}

	NamedStringList & LinkedStringListMap::get(const string & name) {
		for (vector<NamedStringList>::iterator iter = _elems.begin(); iter != _elems.end(); iter++) {
			if (iter->name() == name) {
				return *iter;
			}
		}
		_elems.push_back(NamedStringList(name));
		return *(_elems.rbegin());
	}

	bool LinkedStringListMap::contains(const string & name) const  {
		for (vector<NamedStringList>::const_iterator iter = _elems.begin(); iter != _elems.end(); iter++) {
			if (iter->const_name() == name) {
				return true;
			}
		}
		return false;
	}

	bool LinkedStringListMap::containsIgnoreCase(const string & name) const  {
		for (vector<NamedStringList>::const_iterator iter = _elems.begin(); iter != _elems.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->const_name(), name)) {
				return true;
			}
		}
		return false;
	}

	size_t LinkedStringListMap::size() const {
		return _elems.size();
	}

	void LinkedStringListMap::append(const LinkedStringMap & m) {
		for (size_t i = 0; i < m.size(); i++) {
			_elems.push_back(NamedStringList(m[i].name(), m[i].value()));
		}
	}

	void LinkedStringListMap::append(const map<string, string> & m) {
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			_elems.push_back(NamedStringList(iter->first, iter->second));
		}
	}
	void LinkedStringListMap::erase(const string & name) {
		for (vector<NamedStringList>::iterator iter = _elems.begin(); iter != _elems.end(); iter++) {
			if (iter->name() == name) {
				_elems.erase(iter);
				return;
			}
		}
	}

	map<string, string> LinkedStringListMap::toStdMap() {
		map<string, string> ret;
		for (size_t i = 0; i < _elems.size(); i++) {
			ret[_elems[i].name()] = _elems[i].first();
		}
		return ret;
	}

	NamedStringList & LinkedStringListMap::operator[] (const string & name) {
		return get(name);
	}

	NamedStringList & LinkedStringListMap::operator[] (size_t idx) {
		return _elems[idx];
	}

	NamedStringList LinkedStringListMap::operator[] (size_t idx) const {
		return _elems[idx];
	}

	LinkedStringListMap & LinkedStringListMap::operator+= (const LinkedStringMap & m) {
		append(m);
		return *this;
	}

	LinkedStringListMap LinkedStringListMap::operator+= (const map<string, string> & m) {
		append(m);
		return *this;
	}

	void LinkedStringListMap::operator= (const LinkedStringMap & m) {
		_elems.clear();
		for (size_t i = 0; i < m.size(); i++) {
			_elems.push_back(NamedStringList(m[i].name(), m[i].value()));
		}
	}

	void LinkedStringListMap::operator= (const map<string, string> & m) {
		_elems.clear();
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			_elems.push_back(NamedStringList(iter->first, iter->second));
		}
	}	
	
}
