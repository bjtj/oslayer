#include "Properties.hpp"
#include "Text.hpp"
#include "FileStream.hpp"

namespace osl {

	using namespace std;
	

	Properties::Properties() {
	}

	Properties::~Properties() {
	}

	void Properties::clear() {
		_props.clear();
	}

	void Properties::loadFromFile(const string & filepath) {
        File file(filepath);
		loadFromFile(file);
	}

	void Properties::loadFromFile(File & file) {
		FileStream stream(file, "rb");
		string dump = stream.readFullAsString();
		stream.close();
		parsePropertiesString(dump);
	}

	void Properties::loadFromString(const string & text) {
		parsePropertiesString(text);
	}

	void Properties::writeToFile(const string & filepath) {
        File file(filepath);
		writeToFile(file);
	}

	void Properties::writeToFile(File & file) {
		string ret = convertToPropertiesString();
		FileStream stream(file, "wb");
		stream.write(ret.c_str(), ret.length());
		stream.close();
	}

	void Properties::parsePropertiesString(const string & text) {
		vector<string> lines = Text::split(text, "\n");
		for (vector<string>::iterator iter = lines.begin(); iter != lines.end(); iter++) {
			string & line = *iter;
			if (isMeaningfulLine(line)) {
				KeyValue kv = parseLine(line);
				setProperty(kv.key(), kv.value());
			}
		}
	}

	KeyValue Properties::parseLine(const std::string & line) {
		KeyValue kv;
		size_t sep = line.find("=");
		if (sep == string::npos) {
			kv.key() = Text::trim(line);
		} else {
			kv.key() = Text::trim(line.substr(0, sep));
			kv.value() = Text::trim(line.substr(sep + 1));
		}
		return kv;
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
		for (size_t i = 0; i < _props.size(); i++) {
			KeyValue prop = _props[i];
			ret.append(prop.key());
			ret.append("=");
			ret.append(prop.value());
			ret.append("\n");
		}
		return ret;
	}

	bool Properties::contains(const string & name) const {
		return _props.contains(name);
	}

	bool Properties::hasProperty(const string & name) const {
		return _props.contains(name);
	}

	string Properties::getProperty(const string & name) const {
		for (size_t i = 0; i < _props.size(); i++) {
			if (_props[i].key() == name) {
				return _props[i].value();
			}
		}
		return "";
	}
	string Properties::getProperty(const string & name, const string & def) const {
		if (!_props.contains(name)) {
			return def;
		}
		return getProperty(name);
	}

	int Properties::getIntegerProperty(const std::string & name, int def) const {
		if (!_props.contains(name)) {
			return def;
		}
		return Text::toInt(getProperty(name));
	}

	float Properties::getFloatProperty(const std::string & name, float def) const {
		if (!_props.contains(name)) {
			return def;
		}
		return Text::toFloat(getProperty(name));
	}

	void Properties::setProperty(const string & name, const string & value) {
		_props[name] = value;
	}
	
	void Properties::setProperty(const string & name, int value) {
		setProperty(name, Text::toString(value));
	}

	void Properties::setProperty(const string & name, float value) {
		setProperty(name, Text::toString(value));
	}

	vector<string> Properties::getPropertyNames() {
		vector<string> names;
		for (size_t i = 0; i < _props.size(); i++) {
			names.push_back(_props[i].key());
		}
		return names;
	}

	string & Properties::operator[] (const string & name) {
		return _props[name];
	}

	const string Properties::operator[] (const string & name) const {
		return _props.const_element(name).value();
	}

	map<string, string> Properties::toStandardMap() {
		map<string, string> ret;
		for (size_t i = 0; i < _props.size(); i++) {
			ret[_props[i].key()] = _props[i].value();
		}
		return ret;
	}
}
