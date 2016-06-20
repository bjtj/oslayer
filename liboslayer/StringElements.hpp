#ifndef __STRING_ELEMENT_HPP__
#define __STRING_ELEMENT_HPP__

#include <string>
#include <map>
#include <vector>

namespace UTIL {

	/**
	 * @brief name value
	 */
	class NameValue {
	private:
		std::string _name;
		std::string _value;

	public:
		NameValue();
		NameValue(const std::string & name);
		NameValue(const std::string & name, const std::string & value);
		virtual ~NameValue();
		std::string & name();
		std::string & value();
		const std::string const_name() const;
		const std::string const_value() const;
		bool operator==(const std::string & name) const;
	};
    
    /**
     * @brief name value list
     */
    class NameValueList : public std::vector<NameValue> {
    private:
    public:
        NameValueList();
		NameValueList(const std::vector<NameValue> & lst);
        virtual ~NameValueList();
    };

	/**
	 * @brief 
	 */
	class NamedStringList {
	private:
		std::string _name;
		std::vector<std::string> _strs;
	public:
		NamedStringList();
		NamedStringList(const std::string & name);
		NamedStringList(const std::string & name, const std::string & str);
		NamedStringList(const std::string & name, const std::vector<std::string> & strs);
		virtual ~NamedStringList();
		const std::string const_name() const;
		std::string & name();
		std::vector<std::string> vec_strs();
		std::string & first();
		const std::string const_first() const;
		std::string & last();
		const std::string const_last() const;
		const std::string first_safe(const std::string & def) const;
		const std::string last_safe(const std::string & def) const;
		size_t size();
		void clear();
		std::string & operator[] (size_t idx);
		void operator= (const std::string & str);
		void operator= (const char * str);
		void operator= (const std::vector<std::string> & strs);
		NamedStringList & operator+= (const std::string & str);
		NamedStringList & operator+= (const char * str);
		NamedStringList & operator+= (const std::vector<std::string> & strs);
	};


	/**
	 * @brief string map
	 */
	class StringMap : public std::map<std::string, std::string> {
    private:
	public:
		StringMap();
		virtual ~StringMap();
        std::string & operator[] (const std::string & name);
        std::string get(const std::string & name) const;
	};

	/**
	 * @brief linked string map
	 */
	class LinkedStringMap {
	private:
		std::vector<NameValue> _elements;
	public:
		LinkedStringMap();
		virtual ~LinkedStringMap();
		std::vector<NameValue> elements();
		std::vector<NameValue> toNameValueList() const;
		size_t size() const;
        void clear();
		void erase(const std::string & name);
		bool contains(const std::string & name) const;
		NameValue & get(const std::string & name);
		NameValue const_get(const std::string & name) const;
		void set(const NameValue & nv);
		void append(const LinkedStringMap & m);
		void append(const std::map<std::string, std::string> & m);
		std::map<std::string, std::string> toStdMap();
		NameValue & operator[] (size_t index);
		NameValue operator[] (size_t index) const;
		std::string & operator[] (const std::string & name);
		void operator= (const std::map<std::string, std::string> & m);
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
		NameProperty();
		NameProperty(const std::string & name);
		NameProperty(const std::string & name, const std::string & value);
		virtual ~NameProperty();
		void setName(const std::string & name);
		void setValue(const std::string & value);
		std::string & name();
		std::string & value();
		std::string & getName();
		std::string & getValue();
		const std::string & getName() const;
		const std::string & getValue() const;
		LinkedStringMap & getProperties();
		const LinkedStringMap & getProperties() const;
		std::string & getProperty(const std::string & name);
		std::string getProperty(const std::string & name) const;
		void setProperty(const std::string & name, const std::string & value);
		std::string & operator[] (const std::string & name);
		bool operator==(const std::string & name) const;
		void operator=(const std::string & value);
	};


	/**
	 * @brief linked string map
	 */
	class LinkedStringProperties {
	private:
		std::vector<NameProperty> elements;
	public:
		LinkedStringProperties();
		virtual ~LinkedStringProperties();
		size_t size() const;
        void clear();
		NameProperty & get(const std::string & name);
		const NameProperty & const_get(const std::string & name) const;
		bool contains(const std::string & name) const;
		NameProperty & operator[] (const std::string & name);
		const NameProperty & operator[] (size_t index) const;
		NameProperty & operator[] (size_t index);
	};
    
    /**
     * @brief string list map
     */
    class StringListMap : public std::map<std::string, std::vector<std::string> > {
    private:
    public:
        StringListMap();
        virtual ~StringListMap();
        void append(const std::string & name, const std::string & value);
        std::string getFirstValue(const std::string & name);
        NameValueList toNameValueList() const;
    };

	/**
	 * @brief 
	 */
	class LinkedStringListMap {
	private:
		std::vector<NamedStringList> _elems;
	public:
		LinkedStringListMap();
		virtual ~LinkedStringListMap();
		void clear();
		NamedStringList & get(const std::string & name);
		bool contains(const std::string & name) const;
		bool containsIgnoreCase(const std::string & name) const;
		size_t size() const;
		void append(const LinkedStringMap & m);
		void append(const std::map<std::string, std::string> & m);
		void erase(const std::string & name);
		std::map<std::string, std::string> toStdMap();
		NamedStringList & operator[] (const std::string & name);
		NamedStringList & operator[] (size_t idx);
		NamedStringList operator[] (size_t idx) const;
		LinkedStringListMap & operator+= (const LinkedStringMap & m);
		LinkedStringListMap operator+= (const std::map<std::string, std::string> & m);
		void operator= (const LinkedStringMap & m);
		void operator= (const std::map<std::string, std::string> & m);
	};
}

#endif
