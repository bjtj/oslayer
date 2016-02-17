#include <iostream>
#include <liboslayer/XmlParser.hpp>

using namespace std;
using namespace XML;

int main(int argc, char *args[]) {

	string text = "<div>Hello & World</div> End.";

	cout << text << endl;

	string enc = XmlEncoder::encode(text);
	cout << enc << endl;

	string dec = XmlDecoder::decode(enc);
	cout << dec << endl;
    
    return 0;
}

