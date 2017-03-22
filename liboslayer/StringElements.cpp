#include "StringElements.hpp"
#include "os.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	/**
	 * key value
	 */

	KeyValue::KeyValue() {
	}
	KeyValue::KeyValue(const string & key)
		: _key(key) {
	}
	KeyValue::KeyValue(const string & key, const string & value)
		: _key(key), _value(value){
	}
	KeyValue::~KeyValue() {
	}
	string & KeyValue::key() {
		return _key;
	}
	string & KeyValue::value() {
		return _value;
	}
	const string KeyValue::key() const {
		return _key;
	}
	const string KeyValue::value() const {
		return _value;
	}
	bool KeyValue::operator==(const string & key) const {
		return _key == key;
	}

	/**
	 * string list
	 */

	StringList::StringList() {
	}
	StringList::StringList(size_t n, const string & val)
		: vector<string>(n, val) {
	}
	StringList::~StringList() {
	}
	string & StringList::first() {
		return *begin();
	}
	const string StringList::first() const {
		return *begin();
	}
	const string StringList::first(const string & def) const {
		if (size() == 0) {
			return def;
		}
		return first();
	}
	string & StringList::last() {
		return *rbegin();
	}
	const string StringList::last() const {
		return *rbegin();
	}
	const string StringList::last(const string & def) const {
		if (size() == 0) {
			return def;
		}
		return last();
	}
	void StringList::clearSet(const string & value) {
		clear();
		push_back(value);
	}
	StringList & StringList::operator+= (const string & value) {
		push_back(value);
        return *this;
	}
	StringList & StringList::operator+= (const char * value) {
		push_back(value);
        return *this;
	}
	StringList & StringList::operator+= (const vector<string> & values) {
		insert(end(), values.begin(), values.end());
        return *this;
	}
	void StringList::operator= (const vector<string> & values) {
		clear();
		insert(end(), values.begin(), values.end());
	}

	/**
	 * string map
	 */

	StringMap::StringMap() {
	}
	StringMap::~StringMap() {
	}
	const string StringMap::const_get(const string & key, const string & def) const {
		for (map<string, string>::const_iterator iter = begin(); iter != end(); iter++) {
			if (iter->first == key) {
				return iter->second;
			}
		}
		return def;
	}

	/**
	 * linked string map
	 */

	LinkedStringMap::LinkedStringMap() {
	}
	LinkedStringMap::~LinkedStringMap() {
	}
	vector<KeyValue> & LinkedStringMap::elements() {
		return _elements;
	}
	const vector<KeyValue> LinkedStringMap::const_elements() const {
		return _elements;
	}
	size_t LinkedStringMap::size() const {
		return _elements.size();
	}
	void LinkedStringMap::clear() {
		_elements.clear();
	}
	void LinkedStringMap::erase(const string & key) {
		for (vector<KeyValue>::iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->key() == key) {
				_elements.erase(iter);
				break;
			}
		}
	}
	bool LinkedStringMap::contains(const string & key) const {
		for (vector<KeyValue>::const_iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->key() == key) {
				return true;
			}
		}
		return false;
	}
	bool LinkedStringMap::containsIgnoreCase(const string & key) const {
		for (vector<KeyValue>::const_iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->key(), key)) {
				return true;
			}
		}
		return false;
	}
	KeyValue & LinkedStringMap::element(const string & key) {
		for (vector<KeyValue>::iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->key() == key) {
				return *iter;
			}
		}
		throw "No element found";
	}
	KeyValue LinkedStringMap::const_element(const string & key) const {
		for (vector<KeyValue>::const_iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->key() == key) {
				return *iter;
			}
		}
		throw "No element found";
	}
	KeyValue & LinkedStringMap::obtain(const string & key) {
		for (vector<KeyValue>::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->key() == key) {
				return *iter;
			}
		}
		_elements.push_back(KeyValue(key));
		return element(key);
	}
	void LinkedStringMap::put(const KeyValue & kv) {
		for (vector<KeyValue>::iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->key() == kv.key()) {
				iter->value() = kv.value();
				return;
			}
		}
		_elements.push_back(kv);
	}
	void LinkedStringMap::put(const LinkedStringMap & m) {
		for (size_t i = 0; i < m.size(); i++) {
			put(m[i]);
		}
	}
	void LinkedStringMap::put(const map<string, string> & m) {
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			put(KeyValue(iter->first, iter->second));
		}
	}
	map<string, string> LinkedStringMap::to_map() const {
		map<string, string> m;
		for (vector<KeyValue>::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			m[iter->key()] = iter->value();
		}
		return m;
	}
	KeyValue & LinkedStringMap::operator[] (size_t index) {
		return _elements[index];
	}
	KeyValue LinkedStringMap::operator[] (size_t index) const {
		return _elements[index];
	}
	string & LinkedStringMap::operator[] (const string & key) {
		return obtain(key).value();
	}
	void LinkedStringMap::operator= (const map<string, string> & m) {
		clear();
		put(m);
	}


    /**
     * string list map
     */

	StringListMap::StringListMap() {
	}
	StringListMap::~StringListMap() {
	}


	/**
	 * linked string-lisp map 
	 */

	LinkedStringListMap::LinkedStringListMap() {
	}
	LinkedStringListMap::~LinkedStringListMap() {
	}
	void LinkedStringListMap::clear() {
		_elements.clear();
	}
	bool LinkedStringListMap::contains(const string & key) const {
		for (vector< Named<StringList> >::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->name() == key) {
				return true;
			}
		}
		return false;
	}
	bool LinkedStringListMap::containsIgnoreCase(const string & key) const {
		for (vector< Named<StringList> >::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (Text::equalsIgnoreCase(iter->name(), key)) {
				return true;
			}
		}
		return false;
	}
	size_t LinkedStringListMap::size() const {
		return _elements.size();
	}
	Named<StringList> & LinkedStringListMap::element(const string & key) {
		for (vector< Named<StringList> >::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->name() == key) {
				return *iter;
			}
		}
		throw "No element found";
	}
	Named<StringList> LinkedStringListMap::const_element(const string & key) const {
		for (vector< Named<StringList> >::const_iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->name() == key) {
				return *iter;
			}
		}
		throw "No element found";
	}
	Named<StringList> & LinkedStringListMap::obtain(const string & key) {
		for (vector< Named<StringList> >::iterator iter = _elements.begin(); iter != _elements.end(); iter++) {
			if (iter->name() == key) {
				return *iter;
			}
		}
		_elements.push_back(Named<StringList>(key));
		return element(key);
	}
	void LinkedStringListMap::put(const LinkedStringMap & m) {
		for (size_t i = 0; i < m.size(); i++) {
			if (contains(m[i].key())) {
				element(m[i].key()).obj().clearSet(m[i].value());
			} else {
				_elements.push_back(Named<StringList>(m[i].key(), StringList(1, m[i].value())));
			}
		}
	}
	void LinkedStringListMap::put(const map<string, string> & m) {
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			if (contains(iter->first)) {
				element(iter->first).obj().clearSet(iter->second);
			} else {
				_elements.push_back(Named<StringList>(iter->first, StringList(1, iter->second)));
			}
		}
	}
	void LinkedStringListMap::append(const string & key, const string & value) {
		if (contains(key)) {
			element(key).obj().push_back(value);
		} else {
			_elements.push_back(Named<StringList>(key, StringList(1, value)));
		}
	}
	void LinkedStringListMap::append(const KeyValue & kv) {
		append(kv.key(), kv.value());
	}
	void LinkedStringListMap::append(const LinkedStringMap & m) {
		for (size_t i = 0; i < m.size(); i++) {
			append(m[i]);
		}
	}
	void LinkedStringListMap::append(const map<string, string> & m) {
		for (map<string, string>::const_iterator iter = m.begin(); iter != m.end(); iter++) {
			append(iter->first, iter->second);
		}
	}
	void LinkedStringListMap::erase(const string & key) {
		for (vector< Named<StringList> >::iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			if (iter->name() == key) {
				_elements.erase(iter);
				return;
			}
		}
	}
	map<string, string> LinkedStringListMap::to_first_map(const string & def) const {
		map<string, string> m;
		for (vector< Named<StringList> >::const_iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			m[iter->name()] = iter->obj().first(def);
		}
		return m;
	}
	map<string, string> LinkedStringListMap::to_join_map(const string & glue) const {
		map<string, string> m;
		for (vector< Named<StringList> >::const_iterator iter = _elements.begin();
			 iter != _elements.end(); iter++) {
			m[iter->name()] = Text::join(iter->obj(), glue);
		}
		return m;
	}
	StringList & LinkedStringListMap::operator[] (const string & key) {
		return obtain(key).obj();
	}
	Named<StringList> & LinkedStringListMap::operator[] (size_t idx) {
		return _elements[idx];
	}
	const Named<StringList> LinkedStringListMap::operator[] (size_t idx) const {
		return _elements[idx];
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
		clear();
		put(m);
	}
	void LinkedStringListMap::operator= (const map<string, string> & m) {
		clear();
		put(m);
	}

	/**
	 * key value +attribute
	 */

	KeyValueAttr::KeyValueAttr() {
	}
	KeyValueAttr::KeyValueAttr(const string & key)
		: KeyValue(key) {
	}
	KeyValueAttr::KeyValueAttr(const string & key, const string & value)
		: KeyValue(key, value) {
	}
	KeyValueAttr::KeyValueAttr(const string & key, const string & value, const LinkedStringMap & attrs)
		: KeyValue(key, value), _attrs(attrs) {
	}
	KeyValueAttr::~KeyValueAttr() {
	}
	LinkedStringMap & KeyValueAttr::attrs() {
		return _attrs;
	}
	string & KeyValueAttr::attr(const string & name) {
		return _attrs[name];
	}
	const string KeyValueAttr::attr(const string & name) const {
		return _attrs.const_element(name).value();
	}
	const LinkedStringMap KeyValueAttr::attrs() const {
		return _attrs;
	}
}
