#include <iostream>
#include <liboslayer/XmlParser.hpp>

using namespace std;
using namespace XML;

int main(int argc, char *args[]) {
    string txt =
		"<!DOCTYPE html>"
		"<root>"
		"<u:item type = \"person\" style=\"calm and warm \\\"dude\\\"\">"
		"<name selected>Steve Martin</name>"
		"<date>2015-09-28</date>"
		"<ext />"
		"</u:item>"
		"</root>";

	try {
		XmlDocument doc = DomParser::parse(txt);
		cout << doc.toString() << endl;
		
		XmlNode * node = doc.getRootNode()->getElementByTagName("name");
		cout << "GET: " << node->toString() << endl;
	} catch (const char * e) {
		cout << "exception: " << e << endl;
	} catch (const string & e) {
		cout << "exception: " << e << endl;
	}
    
    return 0;
}
