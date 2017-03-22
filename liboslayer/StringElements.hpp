#ifndef __STRING_ELEMENT_HPP__
#define __STRING_ELEMENT_HPP__

#include <string>
#include <map>
#include <vector>

namespace UTIL {

	/**
	 * named
	 */
	template <typename T>
	class Named {
	private:
		std::string _name;
		T _t;
	public:
		Named();
		Named(const std::string & name);
		Named(const std::string & name, const T & t);
		virtual ~Named();
		std::string & name();
		const std::string name() const;
		T & obj();
		const T obj() const;
	};

	template <typename T>
	Named<T>::Named() {}
	template <typename T>
	Named<T>::Named(const std::string & name) : _name(name) {}
	template <typename T>
	Named<T>::Named(const std::string & name, const T & t) : _name(name), _t(t) {}
	template <typename T>
	Named<T>::~Named() {}
	template <typename T>
	std::string & Named<T>::name() {return _name;}
	template <typename T>
	const std::string Named<T>::name() const {return _name;}
	template <typename T>
	T & Named<T>::obj() {return _t;}
	template <typename T>
	const T Named<T>::obj() const {return _t;}

	/**
	 * key value
	 */
	class KeyValue {
	private:
		std::string _key;
		std::string _value;
	public:
		KeyValue();
		KeyValue(const std::string & key);
		KeyValue(const std::string & key, const std::string & value);
		virtual ~KeyValue();
		std::string & key();
		std::string & value();
		const std::string key() const;
		const std::string value() const;
		bool operator==(const std::string & key) const;
	};

	/**
	 * string list
	 */
	class StringList : public std::vector<std::string> {
	public:
		StringList();
		StringList(size_t n, const std::string & val);
		virtual ~StringList();
		std::string & first();
		const std::string first() const;
		const std::string first(const std::string & def) const;
		std::string & last();
		const std::string last() const;
		const std::string last(const std::string & def) const;
		void clearSet(const std::string & value);
		StringList & operator+= (const std::string & value);
		StringList & operator+= (const char * value);
		StringList & operator+= (const std::vector<std::string> & values);
		void operator= (const std::vector<std::string> & values);
	};

	/**
	 * string map
	 */
	class StringMap : public std::map<std::string, std::string> {
    private:
	public:
		StringMap();
		virtual ~StringMap();
        const std::string const_get(const std::string & key, const std::string & def) const;
	};

	/**
	 * linked string map
	 */
	class LinkedStringMap {
	private:
		std::vector<KeyValue> _elements;
	public:
		LinkedStringMap();
		virtual ~LinkedStringMap();
		std::vector<KeyValue> & elements();
		const std::vector<KeyValue> const_elements() const;
		size_t size() const;
        void clear();
		void erase(const std::string & key);
		bool contains(const std::string & key) const;
		bool containsIgnoreCase(const std::string & key) const;
		KeyValue & element(const std::string & key);
		KeyValue const_element(const std::string & key) const;
		KeyValue & obtain(const std::string & key);
		void put(const KeyValue & kv);
		void put(const LinkedStringMap & m);
		void put(const std::map<std::string, std::string> & m);
		std::map<std::string, std::string> to_map() const;
		KeyValue & operator[] (size_t index);
		KeyValue operator[] (size_t index) const;
		std::string & operator[] (const std::string & key);
		void operator= (const std::map<std::string, std::string> & m);
	};

    /**
     * string list map
     */
    class StringListMap : public std::map<std::string, StringList > {
    private:
    public:
        StringListMap();
        virtual ~StringListMap();
    };

	/**
	 * linked string-list map
	 */
	class LinkedStringListMap {
	private:
		std::vector< Named<StringList> > _elements;
	public:
		LinkedStringListMap();
		virtual ~LinkedStringListMap();
		void clear();
		bool contains(const std::string & key) const;
		bool containsIgnoreCase(const std::string & key) const;
		size_t size() const;
		Named<StringList> & element(const std::string & key);
		Named<StringList> const_element(const std::string & key) const;
		Named<StringList> & obtain(const std::string & key);
		void put(const LinkedStringMap & m);
		void put(const std::map<std::string, std::string> & m);
		void append(const std::string & key, const std::string & value);
		void append(const KeyValue & kv);
		void append(const LinkedStringMap & m);
		void append(const std::map<std::string, std::string> & m);
		void erase(const std::string & key);
		std::map<std::string, std::string> to_first_map(const std::string & def) const;
		std::map<std::string, std::string> to_join_map(const std::string & glue) const;
		StringList & operator[] (const std::string & key);
		Named<StringList> & operator[] (size_t idx);
		const Named<StringList> operator[] (size_t idx) const;
		LinkedStringListMap & operator+= (const LinkedStringMap & m);
		LinkedStringListMap operator+= (const std::map<std::string, std::string> & m);
		void operator= (const LinkedStringMap & m);
		void operator= (const std::map<std::string, std::string> & m);
	};

	/**
	 * key value +attributes
	 */
	class KeyValueAttr : public KeyValue {
	private:
		LinkedStringMap _attrs;
	public:
		KeyValueAttr();
		KeyValueAttr(const std::string & key);
		KeyValueAttr(const std::string & key, const std::string & value);
		KeyValueAttr(const std::string & key, const std::string & value, const LinkedStringMap & attrs);
		virtual ~KeyValueAttr();
		LinkedStringMap & attrs();
		std::string & attr(const std::string & name);
		const std::string attr(const std::string & name) const;
		const LinkedStringMap attrs() const;
	};
}

#endif
