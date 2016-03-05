#ifndef __PROPERTIES_HPP__
#define __PROPERTIES_HPP__

#include "os.hpp"
#include "StringElement.hpp"
#include "FileReaderWriter.hpp"
#include <string>
#include <vector>

namespace UTIL {

	class Properties {
	private:
		LinkedStringProperties properties;
	public:
		Properties();
		virtual ~Properties();

		void loadFromFile(const std::string & filepath);
		void loadFromFile(OS::File & file);
		void writeToFile(const std::string & filepath);
		void writeToFile(OS::File & file);

	private:

		void parsePropertiesString(const std::string & text);
		NameValue parseLine(const std::string & line);
		bool isMeaningfulLine(const std::string & line);
		std::string convertToPropertiesString();

	public:

		void clear();
		std::string getProperty(const std::string & name);
		int getIntegerProperty(const std::string & name, int def = 0);
		void setProperty(const std::string & name, const std::string & value);
		void setProperty(const std::string & name, int value);
		std::vector<std::string> getPropertyNames();
		std::string & operator[] (const std::string & name);
		std::map<std::string, std::string> toStandardMap();
	};

}

#endif
