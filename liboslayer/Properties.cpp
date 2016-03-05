#include "Properties.hpp"
#include "Text.hpp"

namespace UTIL {

	using namespace std;
	using namespace OS;

	Properties::Properties() {
	}

	Properties::~Properties() {
	}

	void Properties::clear() {
		properties.clear();
	}

	void Properties::loadFromFile(const string & filepath) {
        File file(filepath);
		loadFromFile(file);
	}

	void Properties::loadFromFile(File & file) {
		FileReader reader(file);
		string dump = reader.dumpAsString();
		parsePropertiesString(dump);
	}

	void Properties::writeToFile(const string & filepath) {
        File file(filepath);
		writeToFile(file);
	}

	void Properties::writeToFile(File & file) {
		string ret = convertToPropertiesString();
		FileWriter writer(file);
		writer.write(ret.c_str(), ret.length());
	}

	void Properties::parsePropertiesString(const string & text) {
		vector<string> lines = Text::split(text, "\n");
		for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
			string & line = *iter;

			if (isMeaningfulLine(line)) {
				NameValue nv = parseLine(line);
				setProperty(nv.getName(), nv.getValue());
			}
		}
	}

	NameValue Properties::parseLine(const std::string & line) {
		NameValue nv;
		size_t sep = line.find(":");
		if (sep == string::npos) {
			nv.setName(Text::trim(line));
		} else {
			nv.setName(Text::trim(line.substr(0, sep)));
			nv.setValue(Text::trim(line.substr(sep + 1)));
		}
		return nv;
	}

	bool Properties::isMeaningfulLine(const std::string & line) {

		string ltrim = Text::ltrim(line);
		if (ltrim.empty()) { // ignore empty string
			return false;
		}

		if (Text::startsWith(ltrim, "#")) { // ignore comment
			return false;
		}

		return true;
	}

	string Properties::convertToPropertiesString() {
		string ret;
		for (size_t i = 0; i < properties.size(); i++) {
			NameProperty prop = properties[i];
			ret.append(prop.getName());
			ret.append(":");
			ret.append(prop.getValue());
			ret.append("\n");
		}
		return ret;
	}

	string Properties::getProperty(const string & name) {
		return properties.get(name).getValue();
	}

	int Properties::getIntegerProperty(const std::string & name, int def) {
		if (getProperty(name).empty()) {
			return def;
		}
		return Text::toInt(getProperty(name));
	}

	void Properties::setProperty(const string & name, const string & value) {
		properties.get(name).setValue(value);
	}
	
	void Properties::setProperty(const string & name, int value) {
		setProperty(name, Text::toString(value));
	}

	vector<string> Properties::getPropertyNames() {
		vector<string> names;
		for (size_t i = 0; i < properties.size(); i++) {
			names.push_back(properties[i].getName());
		}
		return names;
	}

	string & Properties::operator[] (const string & name) {
		return properties.get(name).getValue();
	}

	map<string, string> Properties::toStandardMap() {
		map<string, string> ret;
		for (size_t i = 0; i < properties.size(); i++) {
			ret[properties[i].name()] = properties[i].value();
		}
		return ret;
	}
}
