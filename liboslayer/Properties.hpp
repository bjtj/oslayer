#ifndef __PROPERTIES_HPP__
#define __PROPERTIES_HPP__

#include "os.hpp"
#include "File.hpp"
#include "StringElements.hpp"
#include <string>
#include <vector>

namespace osl {

	/**
	 * @brief 
	 */
	class Properties {
	private:
		LinkedStringMap _props;
		
	public:
		Properties();
		virtual ~Properties();

		void loadFromFile(const std::string & filepath);
		void loadFromFile(File & file);
		void loadFromString(const std::string & text);
		void writeToFile(const std::string & filepath);
		void writeToFile(File & file);

	private:

		void parsePropertiesString(const std::string & text);
		KeyValue parseLine(const std::string & line);
		bool isMeaningfulLine(const std::string & line);
		std::string convertToPropertiesString();

	public:

		void clear();
		bool contains(const std::string & name) const;
		bool hasProperty(const std::string & name) const;
		std::string getProperty(const std::string & name) const;
		std::string getProperty(const std::string & name, const std::string & def) const;
		int getIntegerProperty(const std::string & name, int def = 0) const;
		float getFloatProperty(const std::string & name, float def = 0) const;
		void setProperty(const std::string & name, const std::string & value);
		void setProperty(const std::string & name, int value);
		void setProperty(const std::string & name, float value);
		std::vector<std::string> getPropertyNames();
		std::string & operator[] (const std::string & name);
		const std::string operator[] (const std::string & name) const;
		std::map<std::string, std::string> toStandardMap();
	};
}

#endif
