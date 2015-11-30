#include <iostream>
#include <liboslayer/Properties.hpp>

using namespace std;
using namespace UTIL;

void s_test_load(const string & path) {
	
	Properties props;
	props.loadFromFile(path);

	cout << " -- File: " << path << endl;

	vector<string> names = props.getPropertyNames();
	for (size_t i = 0; i < names.size(); i++) {
		string & name = names[i];
		string & value = props[name];
		int iVal = props.getIntegerProperty(name);

		cout << name << " : " << value << " (" << iVal << ")" << endl;
	}
}

void s_test_write(const string & path) {

	Properties props;
	
	props.setProperty("port", "8080");
	props.setProperty("docroot", "./docroot");
	props.setProperty("display.name", "sample web server");
	props.setProperty("seperator", " \\n");
	props.setProperty("copyright", "<none>");
	props.setProperty("mime", "html;htm;json;js;css;plain;");

	props.writeToFile(path);

}

int main(int argc, char * args[]) {

	s_test_write("res/config.properties");
	s_test_load("res/config.properties");
	s_test_load("res/sample.properties");

	getchar();

	return 0;
}